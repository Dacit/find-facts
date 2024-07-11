/*  Author:     Fabian Huch, TU Muenchen

Blocks built from command span primitives
*/

package isabelle


import isabelle.XML.Body

import scala.util.parsing.input
import scala.util.parsing.combinator
import scala.util.parsing.input.Position


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
  }

  case class Span(pos: Pos, command: String, kind: String, tree: XML.Tree) {
    override def toString: String =
      if_proper(command, command + if_proper(kind, " (" + kind + ")") + ": ") + XML.content(tree)

    def text: String = XML.content(tree)
    def lines: Int = Library.count_newlines(text)
    def length: Int = Symbol.length(text)
    def range: Symbol.Range = Text.Range(pos.offset, pos.offset + length)

    def is_whitespace = command.isEmpty && kind.isEmpty
    def is_comment = kind == Markup.COMMENT
  }


  /** block structure **/

  sealed trait Block { def spans: List[Span] }

  case class Command(span: Span) extends Block { def spans: List[Span] = List(span) }
  case class Comment(span: Span) extends Block { def spans: List[Span] = List(span) }
  case class Whitespace(span: Span) extends Block { def spans: List[Span] = List(span) }
  case class Decl_Block(span: Span) extends Block { def spans: List[Span] = List(span) }
  case class Block_End(span: Span) extends Block { def spans: List[Span] = List(span) }

  /* proofs */

  sealed trait Prf { def spans: List[Span] }

  case class Prf_Done(span: Span) extends Prf { def spans: List[Span] = List(span) }

  case class Prf_Inner(inner: List[Block], prf: Prf) extends Prf {
    def spans: List[Span] = inner.flatMap(_.spans) ::: prf.spans
  }

  case class Prf_Goal(goal: Goal, prf: Prf) extends Prf {
    def spans: List[Span] = goal.spans ::: prf.spans
  }

  case class Prf_Block(begin: Span, inner: List[Block], end: Span) extends Prf {
    def spans: List[Span] = begin :: inner.flatMap(_.spans) ::: end :: Nil
  }


  /* nested */

  case class Goal(command: Span, prf: Prf) extends Block {
    def spans: List[Span] = command :: prf.spans
  }

  case class Subproof(begin: Span, inner: List[Block], end: Span) extends Block {
    def spans = begin :: inner.flatMap(_.spans) ::: end :: Nil
  }

  case class Theory(before: List[Block], start: Span, inner: List[Block]) {
    def spans: List[Span] = before.flatMap(_.spans) ::: start :: inner.flatMap(_.spans)
  }


  /** parser **/

  trait Parsers extends combinator.Parsers {
    import Keyword.*

    type Elem = Span

    def $$$(kind: String): Parser[Span] = elem(kind, _.kind == kind)

    def comment: Parser[Comment] = elem("comment", _.is_comment) ^^ Comment.apply
    def whitespace: Parser[Whitespace] = elem("whitespace", _.is_whitespace) ^^ Whitespace.apply
    def command: Parser[Command] = (
      $$$(DOCUMENT_BODY) | $$$(DOCUMENT_HEADING) |
      $$$(NEXT_BLOCK) | $$$(PRF_ASM) | $$$(PRF_CHAIN) | $$$(PRF_SCRIPT) | $$$(PRF_DECL) |
      $$$(DIAG) | $$$(THY_STMT) | $$$(THY_LOAD) | $$$(THY_DEFN) | $$$(THY_DECL)) ^^ Command.apply

    def inner: Parser[Block] = comment | whitespace | command

    def prf_done: Parser[Prf_Done] = ($$$(QED_SCRIPT) | $$$(QED)) ^^ Prf_Done.apply
    
    def prf_inner: Parser[Prf_Inner] = rep1(inner) ~ prf ^^ {
      case commands ~ prf => Prf_Inner(commands, prf)
    }
    
    def prf_goal: Parser[Prf_Goal] = ($$$(PRF_SCRIPT_GOAL) | $$$(PRF_SCRIPT_ASM_GOAL)) ~! prf ~ prf ^^ {
      case command ~ prf ~ prf1 => Prf_Goal(Goal(command, prf), prf1)
    }
    
    def prf_block: Parser[Prf_Block] = $$$(PRF_BLOCK) ~! prflevel ~ $$$(QED_BLOCK) ^^ {
      case begin ~ inner ~ end => Prf_Block(begin, inner, end)
    }

    def prf: Parser[Prf] = prf_block | prf_done | prf_inner | prf_goal

    def goal: Parser[Goal] =
      ($$$(PRF_GOAL) | $$$(PRF_ASM_GOAL) |
        $$$(THY_GOAL) | $$$(THY_GOAL_STMT) | $$$(THY_GOAL_DEFN)) ~! prf ^^ {
        case command ~ prf => Goal(command, prf)
      }

    def subproof: Parser[Subproof] = $$$(PRF_OPEN) ~! prflevel ~ $$$(PRF_CLOSE) ^^ {
      case begin ~ inner ~ end => Subproof(begin, inner, end)
    }
    
    def decl_block: Parser[Decl_Block] = $$$(THY_DECL_BLOCK) ^^ Decl_Block.apply
    def block_end: Parser[Block_End] = $$$(THY_END) ^^ Block_End.apply

    def prflevel: Parser[List[Block]] = rep(inner | goal | subproof)
    def toplevel: Parser[List[Block]] = rep(inner | goal | decl_block | block_end)

    def theory: Parser[Theory] = rep(command | comment | whitespace) ~ $$$(THY_BEGIN) ~! toplevel ^^ {
      case before ~ begin ~ inner => Theory(before, begin, inner)
    }
  }

  object Parser extends Parsers {
    class Reader(span: List[Span], line: Int = 0) extends input.Reader[Span] { reader =>
      def pos: input.Position =
        new Position {
          def line: Int = reader.line
          def column: Int = 0
          protected def lineContents: String = ""
        }
      def first: Span = span.head
      def rest: Reader = new Reader(span.tail, line + span.head.lines)
      def atEnd: Boolean = span.isEmpty
    }
  }

  def parse(theory_context: Export.Theory_Context, progress: Progress): Theory = {
    val spans = Span.read_build(theory_context)
    progress.echo("Parsing theory " + theory_context.theory + " with " + spans.length + " spans")
    
    val input = new Parser.Reader(spans)
    val result = Parser.theory(input)
    (result: @unchecked) match {
      case Parser.Success(thy, _) => thy
      case Parser.NoSuccess(s, next) =>
        val span = if_proper(!next.atEnd, ": " + next.first.toString)
        error("Malformed commands: " + s + " at " + next.pos + span)
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

    def read(session_name: String): List[Theory] =
      using(Export.open_session_context0(store, session_name)) { session_context =>
        for {
          db <- session_context.session_db().toList
          name <- deps(session_name).proper_session_theories
          theory_context = session_context.theory(name.theory)
        } yield Thy_Blocks.parse(theory_context, progress)
      }

    val thys = sessions.flatMap(read)
    progress.echo("Parsed " + thys.length + " thys")
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