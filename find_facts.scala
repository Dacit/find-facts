package isabelle


import scala.annotation.tailrec


object Find_Facts {

  case class Block(
    version: String,
    session: String,
    theory: String,
    command: String = "",
    start_line: Int = 0,
    src_before: String = "",
    src: String = "",
    src_after: String = "",
    markup: String = "",
    typs: List[String] = Nil,
    consts: List[String] = Nil,
    thms: List[String] = Nil)

  case class Command(name: String, keyword_kind: String, body: XML.Body)

  def from_xml(body: XML.Body): List[Command] = {
    @tailrec
    def from_tree(tree: XML.Tree): Command =
      tree match {
        case XML.Elem(Markup.Command_Span(arg), body) => Command(arg.name, arg.kind, body)
        case XML.Elem(_, List(tree)) => from_tree(tree)
        case e@XML.Text(t) => Command("", "", List(e))
        case e@XML.Elem(Markup(Markup.COMMENT, Nil), body) => Command(Markup.COMMENT, "", List(e))
        case _ => error("Unknown markup: " + tree)
      }

    body.map(from_tree)
  }

  def get_markup(source: XML.Body): String = {
    def trim(source: XML.Body): XML.Body = source match {
      case XML.Elem(markup, body) :: xs => XML.Elem(markup, trim(body)) :: xs
      case XML.Text(content) :: xs => XML.Text(content.stripTrailing()) :: xs
      case Nil => Nil
    }

    def filter(body: XML.Body): XML.Body =
      body.flatMap {
        case XML.Elem(Markup.Entity(_, _), body) => filter(body)
        case XML.Elem(markup, body) => List(XML.Elem(markup, filter(body)))
        case e => List(e)
      }

    YXML.string_of_body(filter(trim(source.reverse).reverse))
  }

  def find_facts(
    options: Options,
    sessions: List[String],
    version: String = "",
    progress: Progress = new Progress
  ): Unit = {
    val store = Store(options)

    val selection = Sessions.Selection(sessions = sessions)
    val session_structure = Sessions.load_structure(options).selection(selection)
    val deps = Sessions.Deps.load(session_structure)

    val browser_context = Browser_Info.context(session_structure)

    def get_html(source: XML.Body): String = {
      val node_context = Browser_Info.Node_Context.empty
      val elements = Browser_Info.default_elements.copy(entity = Markup.Elements.empty)
      XML.string_of_body(node_context.make_html(elements, source))
    }

    def get_blocks(
      session_name: String,
      name: Document.Node.Name,
      theory_context: Export.Theory_Context,
      snapshot: Document.Snapshot
    ): List[Block] = {
      val content = from_xml(snapshot.xml_markup())
      val thy = Export_Theory.read_theory(theory_context)

      val (i, block, rest) =
        content.foldLeft(0, Block(version, session_name, name.theory), List.empty[Block]) {
          case ((i, block, elems), command) =>
            val src = XML.content(command.body)
            val markup = get_markup(command.body)

            val i1 =
              if ((Keyword.proof_open ++ Keyword.theory_goal).contains(command.keyword_kind)) i + 1
              else if (Keyword.proof_close.contains(command.keyword_kind)) i - 1
              else i

            if (command.name.isEmpty) {
              val block1 = block.copy(src = block.src + src, markup = block.markup + markup)
              (i1, block1, elems)
            } else if (i == 0) {
              val block1 =
                Block(version, session_name, name.theory, command = command.name, start_line =
                  block.start_line + block.src.linesIterator.length, src = src, markup = markup)
              (i1, block1, block :: elems)
            } else {
              val block1 =
                block.copy(command = if (block.command.isEmpty) command.name else block.command,
                  src = block.src + src, markup = block.markup + markup)
              (i1, block1, elems)
            }
        }

      if (i != 0) error("Invalid structure for " + name + ": " + i)
      (block :: rest).reverse
    }

    def read(session_name: String): List[Block] =
      using(Export.open_session_context0(store, session_name)) { session_context =>
        for {
          db <- session_context.session_db().toList
          name <- deps(session_name).proper_session_theories
          theory_context = session_context.theory(name.theory)
          snapshot <- Build.read_theory(theory_context).toList
          block <- get_blocks(session_name, name, theory_context, snapshot)
        } yield block
      }

    val blocks = sessions.flatMap(read)
    progress.echo("Blocks: " + blocks.length)
  }

  val isabelle_tool = Isabelle_Tool("find_facts", "", Scala_Project.here,
  { args =>
    var options = Options.init()

    val getopts = Getopts("""
Usage: isabelle find_facts [OPTIONS]

  Options are:
    -o OPTION    override Isabelle system OPTION (via NAME=VAL or NAME)

  Run find_facts.
""",
        "o:" -> (arg => options = options + arg))

      val sessions = getopts(args)

      val progress = new Console_Progress()

      find_facts(options, sessions, progress = progress)
  })
}

class Find_Facts_Tools extends Isabelle_Scala_Tools(Find_Facts.isabelle_tool)