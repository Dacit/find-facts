/*  Author:     Fabian Huch, TU Muenchen

Blocks built from command span primitives
*/

package isabelle


import scala.util.parsing.input
import scala.util.parsing.combinator
import scala.annotation.tailrec


object Thy_Blocks {
  object Span {
    class Reader(cmds: List[Span], val pos: Pos) extends input.Reader[Span] {
      def first: Span = cmds.head
      def rest: Reader = new Reader(cmds.tail, pos.advance(first))
      def atEnd: Boolean = cmds.isEmpty
    }

    case class Pos(
      file: String,
      line: Int = 0,
      offset: Symbol.Offset = 0
    ) extends input.Position {
      def column = 0
      def lineContents = ""

      def advance(cmd: Span): Pos = advance(XML.content(cmd.body))
      def advance(source: String): Pos = {
        var line1 = line
        var offset1 = offset
        for (s <- Symbol.iterator(source)) {
          if (line1 > 0 && Symbol.is_newline(s)) line1 += 1
          if (offset1 > 0) offset1 += 1
        }
        if (line1 == line && offset1 == offset) this
        else Pos(file, line1, offset1)
      }

      def position(end_offset: Symbol.Offset): Position.T =
        (if (line > 0) Position.Line(line) else Nil) :::
        (if (offset > 0) Position.Offset(offset) else Nil) :::
        (if (end_offset > 0) Position.End_Offset(end_offset) else Nil) :::
        (if (file != "") Position.File(file) else Nil)

      def position(): Position.T = position(0)
      def position(cmd: Span): Position.T = position(advance(cmd).offset)
      def position(source: String): Position.T = position(advance(source).offset)

      override def toString: String = Position.here(position(), delimited = false)
    }
  }

  sealed trait Span { def body: XML.Body }
  case class Command(name: String, kind: String, body: XML.Body) extends Span {
    override def toString: String = name + "(" + kind + "): " + XML.content(body)
  }
  case class Whitespace(body: XML.Body) extends Span {
    override def toString: String = XML.content(body)
  }
  case class Comment(name: String, body: XML.Body) extends Span {
    override def toString: String = XML.content(body)
  }


  /* block structure */

  sealed trait Block {
    def spans: List[Span]
  }

  sealed trait Prf {
    def spans: List[Span]
  }
  case class Prf_Block(begin: Span, inner: List[Block], end: Span) extends Prf {
    def spans: List[Span] = begin :: inner.flatMap(_.spans) ::: end :: Nil
  }
  case class Prf_Step(cmd: Span, prf: Prf) extends Prf {
    def spans = cmd :: prf.spans
  }
  case class Prf_Done(cmd: Span) extends Prf {
    def spans: List[Span] = cmd :: Nil
  }
  case class Prf_Cmd(cmd: Span, prf: Prf) extends Prf {
    def spans: List[Span] = cmd :: prf.spans
  }

  case class Cmd(cmd: Span) extends Block {
    override def toString: String = "########## Command ##########\n" + XML.content(cmd.body)
    def spans: List[Span] = cmd :: Nil
  }

  case class Goal(cmd: Span, prf: Prf) extends Block {
    override def toString: String = "########## Goal ##########\n" + spans.map(span => XML.content(span.body)).mkString
    def spans: List[Span] = cmd :: prf.spans
  }

  case class Decl_Block(begin: Span, inner: List[Block], end: Span) extends Block {
    def spans: List[Span] = begin :: inner.flatMap(_.spans) ::: end :: Nil
  }


  /* parser */

  trait Parsers extends combinator.Parsers {
    import Keyword.*

    type Elem = Span

    def comment: Parser[Span] =
      elem("", { case _: Comment | _: Whitespace => true case _ => false })
    def $$$(kind: String): Parser[Span] =
      elem(kind, { case c: Command if c.kind == kind => true case _ => false})

    def document: Parser[Span] = comment | $$$(DOCUMENT_BODY) | $$$(DOCUMENT_HEADING)
    def command: Parser[Span] =
      $$$(NEXT_BLOCK) | $$$(PRF_ASM) | $$$(PRF_CHAIN) |
      $$$(DIAG) | $$$(THY_STMT) | $$$(THY_LOAD) | $$$(THY_DEFN) | $$$(THY_DECL)
    
    def cmd: Parser[Cmd] = (document | command) ^^ Cmd.apply

    def prf_block: Parser[Prf_Block] = $$$(PRF_BLOCK) ~ inner ~ $$$(QED_BLOCK) ^^ {
      case begin ~ inner ~ end => Prf_Block(begin, inner, end)
    }
    def prf_step: Parser[Prf_Step] = ($$$(PRF_SCRIPT) | $$$(PRF_DECL)) ~ prf ^^ {
      case cmd ~ prf => Prf_Step(cmd, prf)
    }
    def prf_done: Parser[Prf_Done] = ($$$(QED_SCRIPT) | $$$(QED)) ^^ Prf_Done.apply
    def prf_cmd: Parser[Prf_Cmd] = (document | command) ~ prf ^^ {
      case cmd ~ prf => Prf_Cmd(cmd, prf)
    }
    def prf: Parser[Prf] = prf_block | prf_step | prf_done | prf_cmd

    def goal: Parser[Goal] =
      ($$$(PRF_GOAL) | $$$(PRF_ASM_GOAL) | $$$(PRF_SCRIPT_GOAL) | $$$(PRF_SCRIPT_ASM_GOAL) |
        $$$(THY_GOAL) | $$$(THY_GOAL_STMT) | $$$(THY_GOAL_DEFN)) ~ prf ^^ {
        case cmd ~ prf => Goal(cmd, prf)
      }

    def subproof: Parser[Decl_Block] = $$$(PRF_OPEN) ~ inner ~ $$$(PRF_CLOSE) ^^ {
      case begin ~ inner ~ end => Decl_Block(begin, inner, end)
    }
    
    def decl_block: Parser[Block] = $$$(THY_DECL_BLOCK) ~ inner ~ $$$(THY_END) ^^ {
      case begin ~ inner ~ end => Decl_Block(begin, inner, end)
    } | $$$(THY_DECL_BLOCK) ^^ Cmd.apply

    def inner: Parser[List[Block]] = rep(cmd | goal | subproof)
    def toplevel: Parser[List[Block]] = rep(cmd | goal | decl_block)

    def theory: Parser[List[Block]] = toplevel ~ $$$(THY_BEGIN) ~ toplevel ~ $$$(THY_END) ~ toplevel ^^ {
      case before ~ begin ~ inner ~ end ~ after => before ::: Decl_Block(begin, inner, end) :: after
    }
  }

  object Parser extends Parsers

  def parse(theory_context: Export.Theory_Context, progress: Progress): List[Block] = {
    @tailrec
    def from_tree(tree: XML.Tree): Span =
      tree match {
        case XML.Elem(Markup.Command_Span(arg), body) => Command(arg.name, arg.kind, body)
        case XML.Elem(_, List(tree)) => from_tree(tree)
        case e@XML.Text(t) => Whitespace(List(e))
        case e@XML.Elem(Markup(Markup.COMMENT, Nil), body) => Comment(Markup.COMMENT, List(e))
        case _ => error("Unknown markup: " + tree)
      }

    def parse(cmds: List[Span]): List[Block] = {
      val input = new Span.Reader(cmds, Span.Pos(theory_context.theory))
      val result = Parser.theory(input)
      (result: @unchecked) match {
        case Parser.Success(thy, _) => thy
        case Parser.NoSuccess(s, next) =>
          val cmd = next.first
          error("Malformed commands: " + s + " at " + next.pos + ": " + cmd + "\n\n" + cmds
            .collect { case c: Command => c }.mkString("\n\n###############################\n"))
      }
    }

    for {
      snapshot <- Build.read_theory(theory_context).toList
      spans = snapshot.xml_markup().map(from_tree)
      _ = progress.echo("Parsing " + theory_context.theory + " with " + spans.length + " spans")
      block <- parse(spans)
    } yield block
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

    def read(session_name: String): List[Block] =
      using(Export.open_session_context0(store, session_name)) { session_context =>
        for {
          db <- session_context.session_db().toList
          name <- deps(session_name).proper_session_theories
          theory_context = session_context.theory(name.theory)
          block <- Thy_Blocks.parse(theory_context, progress)
        } yield block
      }

    val blocks = sessions.flatMap(read)
    progress.echo("Parsed " + blocks.length + " blocks:\n")
    progress.echo(blocks.mkString("\n"))
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