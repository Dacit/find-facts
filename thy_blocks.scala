/*  Author:     Fabian Huch, TU Muenchen

Blocks built from command span primitives
*/

package isabelle


import isabelle.XML.Body

import scala.util.parsing.input
import scala.util.parsing.combinator
import scala.util.parsing.input.Position
import scala.annotation.tailrec


object Thy_Blocks {
  /** spans **/

  case class Pos(file: String, line: Int = 0, offset: Symbol.Offset = 0) {
    def +(span: Span): Pos = copy(line = line + span.lines, offset + span.length)
  }

  object Span {
    def whitespace(pos: Pos, text: XML.Text): Span = Span(pos, "", "", text)
    def comment(pos: Pos, elem: XML.Elem): Span = Span(pos, "", Markup.COMMENT, elem)

    def read_build(theory_context: Export.Theory_Context): List[Span] = {
      def from_tree(pos: Pos, tree: XML.Tree): Span =
        tree match {
          case elem@XML.Elem(Markup.Command_Span(arg), _) => Span(pos, arg.name, arg.kind, elem)
          case elem@XML.Elem(_, List(tree)) => from_tree(pos, tree).copy(tree = elem)
          case text@XML.Text(t) => whitespace(pos, text)
          case elem@XML.Elem(Markup(Markup.COMMENT, Nil), body) => comment(pos, elem)
          case _ => error("Unknown markup: " + tree)
        }

      val snapshot = Build.read_theory(theory_context).getOrElse(
        error("No snapshot for " + theory_context.theory))

      snapshot.xml_markup().foldLeft((Pos(theory_context.theory), List.empty[Span])) {
        case ((pos, spans), tree) =>
          val span = from_tree(pos, tree)
          (pos + span, spans :+ span)
      }._2
    }

    def contains(tree: XML.Tree, elem: XML.Elem): Boolean =
      tree == elem || (
        tree match {
          case XML.Elem(_, body) => body.exists(contains(_, elem))
          case XML.Text(content) => false
        })
  }

  case class Span(pos: Pos, command: String, kind: String, tree: XML.Tree) extends Block {
    override def toString: String =
      if_proper(command, command + if_proper(kind, " (" + kind + ")") + ": ") + XML.content(tree)

    def spans: List[Span] = List(this)

    def text: String = XML.content(tree)
    def lines: Int = Library.count_newlines(text)
    def length: Int = Symbol.length(text)
    def range: Symbol.Range = Text.Range(pos.offset, pos.offset + length)

    def is_whitespace: Boolean = command.isEmpty && kind.isEmpty
    def is_comment: Boolean = kind == Markup.COMMENT
    def is_of_kind(kinds: Set[String]): Boolean = kinds.contains(kind)

    def has_keyword(keyword: String): Boolean =
      Span.contains(tree,
        XML.Elem(Markup(Markup.KEYWORD2, Markup.Kind(Markup.KEYWORD)), XML.string(keyword)))
  }


  /** block structure **/

  sealed trait Block { def spans: List[Span] }

  case class Thy(inner: List[Block]) extends Block { def spans: List[Span] = inner.flatMap(_.spans) }
  case class Prf(inner: List[Block]) extends Block { def spans: List[Span] = inner.flatMap(_.spans) }
  case class Decl(inner: List[Block]) extends Block { def spans: List[Span] = inner.flatMap(_.spans) }

  /** parser **/

  object Parser {
    object Blocks {
      def empty: Blocks = new Blocks(Nil, Nil)
    }

    case class Blocks(private val stack: List[Block], private val out: List[Block]) {
      def peek: Option[Block] = stack.headOption
      def push(block: Block): Blocks = copy(stack = block :: stack)
      def add(block: Block): Blocks =
        stack match {
          case Nil => copy(out = block :: out)
          case head :: rest =>
            val head1 =
              head match {
                case Thy(inner) => Thy(inner :+ block)
                case Prf(inner) => Prf(inner :+ block)
                case Decl(inner) => Decl(inner :+ block)
                case _ => error("Cannot add to " + head)
              }
            copy(stack = head1 :: rest)
        }

      def pop: Blocks =
        stack match {
          case Nil => error("Nothing to pop")
          case head :: rest => copy(stack = rest).add(head)
        }

      def pop_prfs: Blocks = {
        val blocks1 = pop
        blocks1.stack match {
          case Prf(_) :: _ => blocks1.pop_prfs
          case _ => blocks1
        }
      }

      def output: List[Block] = if (stack.nonEmpty) error("Nonempty stack") else out.reverse
    }

    def parse(spans: List[Span]): List[Block] = {
      import Keyword.*

      def parse1(blocks: Blocks, span: Span): Blocks =
        blocks.peek match {
          case _ if span.is_comment || span.is_whitespace || span.is_of_kind(document) =>
            blocks.add(span)
          case None if span.is_of_kind(theory_begin) => blocks.push(Thy(List(span)))
          case Some(_) if span.is_of_kind(diag) => blocks.add(span)
          case Some(Thy(_)) if span.is_of_kind(theory_goal) => blocks.push(Prf(List(span)))
          case Some(Thy(_)) if span.is_of_kind(theory_block) =>
            val decl = Decl(List(span))
            if (span.has_keyword("begin")) blocks.push(decl) else blocks.add(decl)
          case Some(Thy(_)) if span.is_of_kind(theory_end) => blocks.add(span).pop
          case Some(Thy(_)) if span.is_of_kind(theory_body) => blocks.add(span)
          case Some(Prf(_)) if span.is_of_kind(proof_open) => blocks.push(Prf(List(span)))
          case Some(Prf(_)) if span.is_of_kind(proof_close) => blocks.add(span).pop
          case Some(Prf(_)) if span.is_of_kind(qed_global) => blocks.add(span).pop_prfs
          case Some(Prf(_)) if span.is_of_kind(proof_body) => blocks.add(span)
          case Some(Decl(_)) if span.is_of_kind(theory_goal) => blocks.push(Prf(List(span)))
          case Some(Decl(_)) if span.is_of_kind(theory_block) => blocks.push(Decl(List(span)))
          case Some(Decl(_)) if span.is_of_kind(theory_end) => blocks.add(span).pop
          case Some(Decl(_)) if span.is_of_kind(theory_body) => blocks.add(span)
          case e => error("Unexpected span " + span + " at " + e)
        }

      spans.foldLeft(Blocks.empty)(parse1).output
    }
  }


  /* thy blocks */

  def thy_blocks(
    options: Options,
    sessions: List[String],
    progress: Progress = new Progress
  ): Unit = {
    val store = Store(options)

    val selection = Sessions.Selection(sessions = sessions)
    val session_structure = Sessions.load_structure(options).selection(selection)
    val deps = Sessions.Deps.load(session_structure)

    def read(session_name: String): List[List[Block]] =
      using(Export.open_session_context0(store, session_name)) { session_context =>
        for {
          db <- session_context.session_db().toList
          name <- deps(session_name).proper_session_theories
        } yield {
          val theory_context = session_context.theory(name.theory)
          val spans = Span.read_build(theory_context)
          progress.echo("Parsing theory " + name.theory + " with " + spans.length + " spans")
          Parser.parse(spans)
        }
      }

    val blocks = sessions.flatMap(read)
    progress.echo("Parsed " + blocks.length + " blocks")
  }


  /* isabelle tool */

  val isabelle_tool = Isabelle_Tool("thy_blocks", "", Scala_Project.here,
  { args =>
    var options = Options.init()

    val getopts = Getopts("""
Usage: isabelle thy_blocks [OPTIONS]

  Options are:
    -o OPTION    override Isabelle system OPTION (via NAME=VAL or NAME)

  Parse blocks.
""",
        "o:" -> (arg => options = options + arg))

      val sessions = getopts(args)

      val progress = new Console_Progress()

      thy_blocks(options, sessions, progress = progress)
  })
}

class Thy_Blocks_Tools extends Isabelle_Scala_Tools(Thy_Blocks.isabelle_tool)