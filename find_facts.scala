package isabelle


import scala.annotation.tailrec
import scala.collection.immutable.TreeMap


object Find_Facts {
  val index_name = "find_facts"


  /** blocks **/

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
    thms: List[String]
  ) {
    def theory_base: String = Long_Name.base_name(theory)
    def kinds: List[String] =
      (if (typs.nonEmpty) List(Export_Theory.Kind.THM) else Nil) :::
      (if (consts.nonEmpty) List(Export_Theory.Kind.CONST) else Nil) :::
      (if (thms.nonEmpty) List(Export_Theory.Kind.THM) else Nil)
  }

  case class Blocks(num: Long, elems: List[Block])


  /** Solr data model **/

  object private_data extends Solr.Data("find_facts") {
    /* types */

    val symbol_codes =
      for {
        entry <- Symbol.symbols.entries
        code <- entry.decode.toList
        input <- entry.symbol :: entry.abbrevs
      } yield input -> code

    val replacements =
      for ((symbol, codes) <- symbol_codes.groupMap(_._1)(_._2).toList if codes.length == 1)
      yield symbol -> Library.the_single(codes)

    val Special_Char = """(.*[(){}\[\].,:"].*)""".r
    val Arrow = """(.*=>.*)""".r

    val synonyms =
      for {
        (symbol, code) <- symbol_codes
        if !Special_Char.matches(symbol) && !Arrow.matches(symbol)
      } yield symbol + " => " + code

    override lazy val more_config = Map(Path.basic("synonyms.txt") -> synonyms.mkString("\n"))

    object Types {
      private val strip_html = Solr.Class("charFilter", "HTMLStripCharFilterFactory")
      private val replace_symbol_chars =
        replacements.collect {
          case (Special_Char(pattern), code) =>
            Solr.Class(
              "charFilter", "PatternReplaceCharFilterFactory",
              List("pattern" -> ("\\Q" + pattern + "\\E"), "replacement" -> code))
        }

      private val symbol_pattern =
         """\s*(::|[(){}\[\].,:"]|[^\p{ASCII}]|((?![^\p{ASCII}])[^(){}\[\].,:"\s])+)\s*""".r // TODO check

      private val tokenize =
        Solr.Class("tokenizer", "WhitespaceTokenizerFactory", List("rule" -> "java"))
      private val tokenize_symbols =
        Solr.Class("tokenizer", "PatternTokenizerFactory",
          List("pattern" -> symbol_pattern.toString, "group" -> "1"))

      private val to_lower = Solr.Class("filter", "LowerCaseFilterFactory")
      private val add_ascii =
        Solr.Class("filter", "ASCIIFoldingFilterFactory", List("preserveOriginal" -> "true"))
      private val delimit_words =
        Solr.Class("filter", "WordDelimiterGraphFilterFactory", List(
          "splitOnCaseChange" -> "0", "stemEnglishPossessive" -> "0", "preserveOriginal" -> "1"))
      private val flatten = Solr.Class("filter", "FlattenGraphFilterFactory")
      private val replace_symbols =
        Solr.Class("filter", "SynonymGraphFilterFactory", List("synonyms" -> "synonyms.txt"))
      private val replace_special_symbols =
        replacements.collect {
          case (Arrow(arrow), code) =>
            Solr.Class("filter", "PatternReplaceFilterFactory",
              List("pattern" -> ("\\Q" + arrow + "\\E"), "replacement" -> code))
        }

      val source =
        Solr.Type("name", "TextField", Nil, List(
          XML.Elem(Markup("analyzer", List("type" -> "index")),
            List(strip_html, tokenize_symbols, to_lower, add_ascii, delimit_words, flatten)),
          XML.Elem(
            Markup("analyzer", List("type" -> "query")),
              replace_symbol_chars ::: tokenize_symbols :: replace_symbols ::
                replace_special_symbols ::: to_lower :: Nil)))

      val name =
        Solr.Type("source", "TextField", Nil, List(
          XML.Elem(Markup("analyzer", List("type" -> "index")),
            List(tokenize, to_lower, delimit_words, flatten)),
          XML.Elem(Markup("analyzer", List("type" -> "query")), List(tokenize, to_lower))))

      val text = Solr.Type("text", "TextField")
    }


    /* fields */

    object Fields {
      val id = Solr.Field("id", Solr.Type.string).make_unique_key
      val version = Solr.Field("version", Solr.Type.string, Solr.Column_Wise(true))
      val session = Solr.Field("session", Types.name)
      val session_facet = Solr.Field("session_facet", Solr.Type.string, Solr.Stored(false))
      val theory = Solr.Field("theory", Types.name)
      val theory_base = Solr.Field("theory_base", Solr.Type.string)
      val theory_facet = Solr.Field("theory_facet", Solr.Type.string, Solr.Stored(false))
      val file = Solr.Field("file", Solr.Type.string)
      val url = Solr.Field("url", Solr.Type.string)
      val command = Solr.Field("command", Solr.Type.string, Solr.Column_Wise(true))
      val start_line = Solr.Field("start_line", Solr.Type.int)
      val src_before = Solr.Field("src_before", Solr.Type.string, Solr.Indexed(false))
      val src_after = Solr.Field("src_after", Solr.Type.string, Solr.Indexed(false))
      val src = Solr.Field("src", Types.source)
      val markup = Solr.Field("markup", Types.text, Solr.Indexed(false))
      val html = Solr.Field("html", Types.text, Solr.Indexed(false))
      val typs = Solr.Field("typs", Types.name, Solr.Multi_Valued(true))
      val consts = Solr.Field("consts", Types.name, Solr.Multi_Valued(true))
      val thms = Solr.Field("thms", Types.name, Solr.Multi_Valued(true))
      val kinds =
        Solr.Field("kinds", Solr.Type.string, Solr.Multi_Valued(true) ::: Solr.Column_Wise(true))
    }

    override lazy val fields: Solr.Fields = Solr.Fields(
      Fields.id, Fields.version, Fields.session, Fields.session_facet, Fields.theory,
      Fields.theory_base, Fields.theory_facet, Fields.file, Fields.url, Fields.command,
      Fields.start_line, Fields.src_before, Fields.src_after, Fields.src, Fields.markup,
      Fields.html, Fields.typs, Fields.consts, Fields.thms, Fields.kinds)


    /* operations */

    def read_domain(db: Solr.Database, restrict: Solr.Source = Solr.any): Set[String] =
      db.execute_query(Fields.id, List(Fields.id), restrict).map(_.string(Fields.id)).toSet

    def read_blocks(db: Solr.Database, query: Solr.Source = Solr.any, num: Int = -1): Blocks = {
      val results = db.execute_query(Fields.id, stored_fields, query)
      val it = if (num < 0) results else results.take(num)
      val blocks = it.map { res =>
        val id = res.string(Fields.id)
        val version = res.string(Fields.version)
        val session = res.string(Fields.session)
        val theory = res.string(Fields.theory)
        val file = res.string(Fields.file)
        val url = res.string(Fields.url)
        val command = res.string(Fields.command)
        val start_line = res.int(Fields.start_line)
        val src_before = res.string(Fields.src_before)
        val src = res.string(Fields.src)
        val src_after = res.string(Fields.src_after)
        val markup = res.string(Fields.markup)
        val html = res.string(Fields.html)
        val typs = res.list_string(Fields.typs)
        val consts = res.list_string(Fields.consts)
        val thms = res.list_string(Fields.thms)

        Block(id = id, version = version, session = session, theory = theory, file = file,
          url = url, command = command, start_line = start_line, src_before = src_before, src = src,
          src_after = src_after, markup = markup, html = html, typs = typs, consts = consts,
          thms = thms)
      }

      Blocks(results.num_found, blocks.toList)
    }

    def update_theory(db: Solr.Database, name: String, blocks: List[Block]): Unit =
      db.transaction {
        val delete =
          read_domain(db, Solr.filter(Fields.theory, Solr.phrase(name))) -- blocks.map(_.id)

        if (delete.nonEmpty) db.execute_batch_delete(delete.toList)

        db.execute_batch_insert(
          for (block <- blocks) yield { (doc: Solr.Document) =>
            doc.string(Fields.id) = block.id
            doc.string(Fields.version) = block.version
            doc.string(Fields.session) = block.session
            doc.string(Fields.session_facet) = block.session
            doc.string(Fields.theory) = block.theory
            doc.string(Fields.theory_base) = block.theory_base
            doc.string(Fields.theory_facet) = block.theory
            doc.string(Fields.file) = block.file
            doc.string(Fields.url) = block.url
            doc.string(Fields.command) = block.command
            doc.int(Fields.start_line) = block.start_line
            doc.string(Fields.src_before) = block.src_before
            doc.string(Fields.src) = block.src
            doc.string(Fields.src_after) = block.src_after
            doc.string(Fields.markup) = block.markup
            doc.string(Fields.html) = block.html
            doc.string(Fields.typs) = block.typs
            doc.string(Fields.consts) = block.consts
            doc.string(Fields.thms) = block.thms
            doc.string(Fields.kinds) = block.kinds
          })
      }
  }


  /** indexing **/

  def read_blocks(
    version: String,
    name: Document.Node.Name,
    browser_info_context: Browser_Info.Context,
    document_info: Document_Info,
    theory_context: Export.Theory_Context
  ): List[Block] = {
    val elements = Browser_Info.default_elements.copy(entity = Markup.Elements.empty)
    val node_context = Browser_Info.Node_Context.empty

    val blocks = Thy_Blocks.read_blocks(theory_context)
    val full_src = blocks.map(_.source).mkString
    val document = Line.Document(full_src)
    val num_lines = document.lines.length

    def check(block: Thy_Blocks.Block): Unit = {
      if (block.spans.isEmpty) error("Empty block: " + block)
      val line_range = document.range(block.source_range)
      if (line_range.start.line != block.pos.line)
        error("Inconsistent start: " + line_range.start.line + ", " + block.pos.line)
      if (line_range.stop.line != block.pos.line + block.lines)
        error("Inconsistent stop: " + line_range.stop.line + ", " + (block.pos.line + block.lines))
      if (block.source != block.source_range.substring(full_src))
        error("Inconsistent src: " + block.source + ", " + block.source_range.substring(full_src))
    }

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

    def get_source(start: Line.Position, stop: Line.Position): String =
      Text.Range(document.offset(start).get, document.offset(stop).get).substring(document.text)

    val thy_entities =
      for {
        entity <- Export_Theory.read_theory(theory_context).entity_iterator.toList
        if Path.explode(entity.file).canonical == name.path.canonical
      } yield entity

    val entities = TreeMap.from(thy_entities.groupBy(_.range.start).toList)

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
      check(block)

      val symbol_range = block.symbol_range
      val id = theory + "#" + symbol_range.start + ".." + symbol_range.stop
      val line_range = document.range(block.source_range)

      val src_before =
        get_source(Line.Position((line_range.start.line - 5).max(0)), line_range.start)
      val src = Symbol.decode(block.source)
      val src_after =
        get_source(line_range.stop, Line.Position((line_range.stop.line + 5).min(num_lines)))

      val markup = YXML.string_of_body(sanitize_body(block.body))
      val html = XML.string_of_body(node_context.make_html(elements, block.body))

      val maybe_entities =
        entities.range(symbol_range.start, symbol_range.stop).values.toList.flatten.distinct
      def get_entities(kind: String): List[String] =
        for {
          entity <- maybe_entities
          if entity.export_kind == kind
          if symbol_range.contains(entity.range)
        } yield entity.name

      val typs = get_entities(Export_Theory.Kind.TYPE)
      val consts = get_entities(Export_Theory.Kind.CONST)
      val thms = get_entities(Export_Theory.Kind.THM)

      Block(id = id, version = version, session = session, theory = theory, file = name.node,
        url = url, command = block.command, start_line = line_range.start.line, src_before =
          src_before, src = src, src_after = src_after, markup = markup, html = html, typs = typs,
        consts = consts, thms = thms)
    }
  }

  def index_blocks(
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

    Isabelle_System.rm_tree(Solr.solr_home)

    val blocks =
      using(Solr.open_database(Find_Facts.private_data)) { db =>
        using(Export.open_database_context(store)) { database_context =>
          val document_info = Document_Info.read(database_context, deps, sessions)
          sessions.foreach(session =>
            using(database_context.open_session0(session)) { session_context =>
              progress.echo("Session " + session + " ...")
              deps(session).proper_session_theories.foreach { name =>
                progress.echo("Theory " + name.theory + " ...")
                val theory_context = session_context.theory(name.theory)
                val blocks =
                  read_blocks(version, name, browser_info_context, document_info, theory_context)
                Find_Facts.private_data.update_theory(db, theory_context.theory, blocks)
              }
            })
        }
        Find_Facts.private_data.read_blocks(db)
      }

    val num_typs = blocks.elems.flatMap(_.typs).distinct.length
    val num_consts = blocks.elems.flatMap(_.consts).distinct.length
    val num_thms = blocks.elems.flatMap(_.thms).distinct.length
    progress.echo("Blocks: " + blocks.num +
      ", typs: " + num_typs + ", consts: " + num_consts + ", thms: " + num_thms)
  }


  /* isabelle tool wrapper */

  val isabelle_tool = Isabelle_Tool("find_facts_index", "index sessions for find_facts",
    Scala_Project.here,
    { args =>
      var options = Options.init()

      val getopts = Getopts("""
  Usage: isabelle find_facts_index [OPTIONS] [SESSIONS ...]

    Options are:
      -o OPTION    override Isabelle system OPTION (via NAME=VAL or NAME)

    Index sessions for find_facts.
  """,
          "o:" -> (arg => options = options + arg))

        val sessions = getopts(args)

        val progress = new Console_Progress()

        index_blocks(options, sessions, progress = progress)
    })


  /** querying **/

  /* find facts */

  def find_facts(options: Options, query: String, progress: Progress = new Progress): Unit = {
    using(Solr.open_database(Find_Facts.private_data)) { db =>
      val blocks = Find_Facts.private_data.read_blocks(db, query, 10)
      progress.echo("Found " + blocks.num + " results. Top 10:")
      blocks.elems.foreach(block => progress.echo(block.toString))
    }
  }

  val isabelle_tool1 = Isabelle_Tool("find_facts", "run find_facts query", Scala_Project.here,
  { args =>
    var options = Options.init()

    val getopts = Getopts("""
Usage: isabelle find_facts [OPTIONS] QUERY

  Options are:
    -o OPTION    override Isabelle system OPTION (via NAME=VAL or NAME)

  Run a find_facts query.
""",
        "o:" -> (arg => options = options + arg))

    val more_args = getopts(args)
    val query = more_args match {
      case query :: Nil => query
      case _ => getopts.usage()
    }

    val progress = new Console_Progress()

    find_facts(options, query, progress)
  })

}
