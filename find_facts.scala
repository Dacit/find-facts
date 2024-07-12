package isabelle


import scala.annotation.tailrec
import scala.collection.immutable.TreeMap


object Find_Facts {

  case class Block(
    id: String,
    version: String,
    session: String,
    theory: String,
    file: String,
    url: String,
    command: String,
    start_line: Int,
    src_before: String,
    src: String,
    src_after: String,
    markup: String,
    html: String,
    typs: List[String],
    consts: List[String],
    thms: List[String])

  def sanitize_body(body: XML.Body): XML.Body = {
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

    filter(trim(body.reverse).reverse)
  }

  def get_blocks(
    version: String,
    name: Document.Node.Name,
    browser_info_context: Browser_Info.Context,
    document_info: Document_Info,
    theory_context: Export.Theory_Context
  ): List[Block] = {
    val elements = Browser_Info.default_elements.copy(entity = Markup.Elements.empty)
    val node_context = Browser_Info.Node_Context.empty

    val blocks = Thy_Blocks.read_blocks(theory_context)
    val lines = Line.Document(blocks.map(_.source).mkString)

    def get_source(start_line: Int, stop_line: Int): String = {
      val start_pos = Line.Position(start_line.max(0))
      val stop_pos = Line.Position(stop_line.min(lines.lines.length))
      Text.Range(lines.offset(start_pos).get, lines.offset(stop_pos).get).substring(lines.text)
    }

    val entities = TreeMap.from(
      for {
        entity <- Export_Theory.read_theory(theory_context).entity_iterator
        if Path.explode(entity.file).canonical == name.path.canonical
        offset <- entity.range.start until entity.range.stop
      } yield offset -> entity)

    def expand_block(block: Thy_Blocks.Block): List[Thy_Blocks.Block] =
      block match {
        case s: Thy_Blocks.Span if s.is_whitespace => Nil
        case Thy_Blocks.Thy(inner) => inner.flatMap(expand_block)
        case e@Thy_Blocks.Decl(inner) => e :: inner.flatMap(expand_block)
        case _ => List(block)
      }

    val session = theory_context.session_context.session_name
    val theory = theory_context.theory
    val theory_info =
      document_info.theory_by_name(session, theory).getOrElse(error("No info for theory " + theory))
    val url = browser_info_context.theory_html(theory_info).implode

    blocks.flatMap(expand_block).map { block =>
      if (block.spans.isEmpty) error("Empty block: " + block)

      val symbol_range = block.symbol_range
      val id = theory + "#" + symbol_range.start + ".." + symbol_range.stop
      val line_range = lines.range(block.source_range)
      val src_before = get_source(line_range.start.line - 5, line_range.start.line)
      val src_after = get_source(line_range.stop.line, line_range.stop.line + 5)
      val markup = YXML.string_of_body(sanitize_body(block.body))
      val html = XML.string_of_body(node_context.make_html(elements, block.body))

      val maybe_entities =
        entities.range(symbol_range.start, symbol_range.stop).values.toList.distinct
      def get_entities(kind: String): List[String] =
        for {
          entity <- maybe_entities
          if entity.export_kind == kind
          if symbol_range.contains(entity.range)
        } yield entity.name

      val typs = get_entities(Export_Theory.Kind.TYPE)
      val consts = get_entities(Export_Theory.Kind.CONST)
      val thms = get_entities(Export_Theory.Kind.THM)

      Block(id = id, version = version, session = session, theory = Long_Name.base_name(theory),
        file = name.node, url = url, command = block.command, start_line = line_range.start.line,
        src_before = src_before, src = Symbol.decode(block.source), src_after = src_after,
        markup = markup, html = html, typs = typs, consts = consts, thms = thms)
    }
  }


  /* find facts */

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
    val browser_info_context = Browser_Info.context(session_structure)

    val blocks =
      using(Export.open_database_context(store)) { database_context =>
        val document_info = Document_Info.read(database_context, deps, sessions)
        sessions.flatMap(session =>
          using(database_context.open_session0(session)) { session_context =>
            progress.echo("Session " + session + " ...")
            deps(session).proper_session_theories.flatMap { name =>
              progress.echo("Theory " + name.theory + " ...")
              val theory_context = session_context.theory(name.theory)
              get_blocks(version, name, browser_info_context, document_info, theory_context)
            }
          })
      }

    val num_typs = blocks.flatMap(_.typs).distinct.length
    val num_consts = blocks.flatMap(_.consts).distinct.length
    val num_thms = blocks.flatMap(_.thms).distinct.length
    progress.echo("Blocks: " + blocks.length +
      ", typs: " + num_typs + ", consts: " + num_consts + ", thms: " + num_thms)
  }

  val isabelle_tool = Isabelle_Tool("find_facts", "", Scala_Project.here,
  { args =>
    var options = Options.init()

    val getopts = Getopts("""
Usage: isabelle find_facts [OPTIONS]

  Options are:
    -o OPTION    override Isabelle system OPTION (via NAME=VAL or NAME)

  Run find_facts. HOL: Blocks: 22995, typs: 101, consts: 3513, thms: 32908
""",
        "o:" -> (arg => options = options + arg))

      val sessions = getopts(args)

      val progress = new Console_Progress()

      find_facts(options, sessions, progress = progress)
  })
}

class Find_Facts_Tools extends Isabelle_Scala_Tools(Find_Facts.isabelle_tool)