/*  Title:      Tools/Find_Facts.scala
    Author:     Fabian Huch, TU Muenchen

Full-text search engine for Isabelle (including web server), using Solr as backend.
*/
package isabelle


import scala.annotation.tailrec
import scala.collection.immutable.TreeMap
import scala.collection.mutable


object Find_Facts {
  /** blocks **/

  object Kind {
    val CONST = "constant"
    val TYPE = "type"
    val THM = "fact"
  }

  case class Block(
    id: String,
    version: Long,
    chapter: String,
    session: String,
    theory: String,
    file: Path,
    url_path: Path,
    command: String,
    start_line: Int,
    src_before: String,
    src: String,
    src_after: String,
    xml: XML.Body,
    html: String,
    consts: List[String],
    typs: List[String],
    thms: List[String]
  ) {
    def kinds: List[String] =
      (if (typs.nonEmpty) List(Kind.TYPE) else Nil) :::
      (if (consts.nonEmpty) List(Kind.CONST) else Nil) :::
      (if (thms.nonEmpty) List(Kind.THM) else Nil)
    def names: List[String] = (typs ::: consts ::: thms).distinct
  }

  case class Blocks(num_found: Long, blocks: List[Block], cursor: String)


  /** queries */

  enum Atom {
    case Exact(s: String) extends Atom
    case Value(s: String) extends Atom
  }

  enum Field {
    case chapter, session, theory, command, source, names, consts, typs, thms, kinds
  }

  sealed trait Filter
  case class Field_Filter(field: Field, either: List[Atom]) extends Filter
  case class Any_Filter(either: List[Atom]) extends Filter {
    def fields: List[Field] = List(Field.session, Field.theory, Field.source, Field.names)
  }

  case class Select(field: Field, values: List[String])

  object Query {
    def apply(filters: Filter*): Query = new Query(filters.toList)
  }

  case class Query(
    filters: List[Filter] = Nil,
    exclude: List[Filter] = Nil,
    selects: List[Select] = Nil)


  /* stats and facets */

  case class Stats(
    results: Long,
    sessions: Long,
    theories: Long,
    commands: Long,
    consts: Long,
    typs: Long,
    thms: Long)

  case class Facets(
    chapter: Map[String, Long],
    session: Map[String, Long],
    theory: Map[String, Long],
    command: Map[String, Long],
    kinds: Map[String, Long],
    consts: Map[String, Long],
    typs: Map[String, Long],
    thms: Map[String, Long])


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
         """\s*(::|[(){}\[\].,:"]|[^\p{ASCII}]|((?![^\p{ASCII}])[^(){}\[\].,:"\s])+)\s*""".r

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
      val version = Solr.Field("version", Solr.Type.long, Solr.Column_Wise(true))
      val chapter = Solr.Field("chapter", Solr.Type.string, Solr.Column_Wise(true))
      val session = Solr.Field("session", Types.name)
      val session_facet = Solr.Field("session_facet", Solr.Type.string, Solr.Stored(false))
      val theory = Solr.Field("theory", Types.name)
      val theory_facet = Solr.Field("theory_facet", Solr.Type.string, Solr.Stored(false))
      val file = Solr.Field("file", Solr.Type.string, Solr.Indexed(false))
      val url_path = Solr.Field("url_path", Solr.Type.string, Solr.Indexed(false))
      val command = Solr.Field("command", Solr.Type.string, Solr.Column_Wise(true))
      val start_line = Solr.Field("start_line", Solr.Type.int, Solr.Column_Wise(true))
      val src_before = Solr.Field("src_before", Solr.Type.string, Solr.Indexed(false))
      val src_after = Solr.Field("src_after", Solr.Type.string, Solr.Indexed(false))
      val src = Solr.Field("src", Types.source)
      val xml = Solr.Field("xml", Solr.Type.bytes, Solr.Indexed(false))
      val html = Solr.Field("html", Solr.Type.bytes, Solr.Indexed(false))
      val consts = Solr.Field("consts", Types.name, Solr.Multi_Valued(true))
      val consts_facet =
        Solr.Field("consts_facet", Solr.Type.string, Solr.Multi_Valued(true) ::: Solr.Stored(false))
      val typs = Solr.Field("typs", Types.name, Solr.Multi_Valued(true))
      val typs_facet =
        Solr.Field("typs_facet", Solr.Type.string, Solr.Multi_Valued(true) ::: Solr.Stored(false))
      val thms = Solr.Field("thms", Types.name, Solr.Multi_Valued(true))
      val thms_facet =
        Solr.Field("thms_facet", Solr.Type.string, Solr.Multi_Valued(true) ::: Solr.Stored(false))
      val names = Solr.Field("names", Types.name, Solr.Multi_Valued(true) ::: Solr.Stored(false))
      val kinds =
        Solr.Field("kinds", Solr.Type.string,
          Solr.Multi_Valued(true) ::: Solr.Column_Wise(true) ::: Solr.Stored(false))
    }

    lazy val fields: Solr.Fields = Solr.Fields(
      Fields.id, Fields.version, Fields.chapter, Fields.session, Fields.session_facet, Fields.theory,
      Fields.theory_facet, Fields.file, Fields.url_path, Fields.command, Fields.start_line,
      Fields.src_before, Fields.src_after, Fields.src, Fields.xml, Fields.html, Fields.consts,
      Fields.consts_facet, Fields.typs, Fields.typs_facet, Fields.thms, Fields.thms_facet,
      Fields.names, Fields.kinds)


    /* operations */

    def read_domain(db: Solr.Database, q: Solr.Source): Set[String] =
      db.execute_query(Fields.id, List(Fields.id), None, 100000,
        { results =>
          results.map(_.string(Fields.id)).toSet
        }, q = q)

    def read_block(res: Solr.Result): Block = {
      val id = res.string(Fields.id)
      val version = res.long(Fields.version)
      val chapter = res.string(Fields.chapter)
      val session = res.string(Fields.session)
      val theory = res.string(Fields.theory)
      val file = Path.explode(res.string(Fields.file))
      val url_path = Path.explode(res.string(Fields.url_path))
      val command = res.string(Fields.command)
      val start_line = res.int(Fields.start_line)
      val src_before = res.string(Fields.src_before)
      val src = res.string(Fields.src)
      val src_after = res.string(Fields.src_after)
      val xml = YXML.parse_body(res.bytes(Fields.xml))
      val html = res.bytes(Fields.html).text
      val consts = res.list_string(Fields.consts)
      val typs = res.list_string(Fields.typs)
      val thms = res.list_string(Fields.thms)

      Block(id = id, version = version, chapter = chapter, session = session, theory = theory,
        file = file, url_path = url_path, command = command, start_line = start_line, src_before =
        src_before, src = src, src_after = src_after, xml = xml, html = html, consts = consts,
        typs = typs, thms = thms)
    }

    def read_blocks(
      db: Solr.Database,
      q: Solr.Source,
      fq: List[Solr.Source],
      cursor: Option[String] = None,
      max_results: Int = 10
    ): Blocks =
      db.execute_query(Fields.id, stored_fields, cursor, max_results,
        { results =>
          val next_cursor = results.next_cursor
          val blocks = results.map(read_block).toList
          Blocks(results.num_found, blocks, next_cursor)
        }, q = q, fq = fq, more_chunks = 0)

    def stream_blocks(
      db: Solr.Database,
      q: Solr.Source,
      stream: Iterator[Block] => Unit,
      cursor: Option[String] = None,
    ): Unit =
      db.execute_query(Fields.id, stored_fields, cursor, 10000,
        { results =>
          stream(results.map(read_block))
        }, q = q)

    def update_theory(db: Solr.Database, theory_name: String, blocks: List[Block]): Unit =
      db.transaction {
        val delete =
          read_domain(db, Solr.filter(Fields.theory, Solr.phrase(theory_name))) -- blocks.map(_.id)

        if (delete.nonEmpty) db.execute_batch_delete(delete.toList)

        db.execute_batch_insert(
          for (block <- blocks) yield { (doc: Solr.Document) =>
            doc.string(Fields.id) = block.id
            doc.long(Fields.version) = block.version
            doc.string(Fields.chapter) = block.chapter
            doc.string(Fields.session) = block.session
            doc.string(Fields.session_facet) = block.session
            doc.string(Fields.theory) = block.theory
            doc.string(Fields.theory_facet) = block.theory
            doc.string(Fields.file) = block.file.implode
            doc.string(Fields.url_path) = block.url_path.implode
            doc.string(Fields.command) = block.command
            doc.int(Fields.start_line) = block.start_line
            doc.string(Fields.src_before) = block.src_before
            doc.string(Fields.src) = block.src
            doc.string(Fields.src_after) = block.src_after
            doc.bytes(Fields.xml) = YXML.bytes_of_body(block.xml)
            doc.bytes(Fields.html) = Bytes(block.html)
            doc.string(Fields.consts) = block.consts
            doc.string(Fields.consts_facet) = block.consts
            doc.string(Fields.typs) = block.typs
            doc.string(Fields.typs_facet) = block.typs
            doc.string(Fields.thms) = block.thms
            doc.string(Fields.thms_facet) = block.thms
            doc.string(Fields.names) = block.names
            doc.string(Fields.kinds) = block.kinds
          })
      }

    def read_theory(db: Solr.Database, theory_name: String): List[Block] = {
      val blocks = new mutable.ListBuffer[Block]
      stream_blocks(db, Solr.filter(Fields.theory_facet, Solr.phrase(theory_name)), {
        res => blocks ++= res
      })
      blocks.toList
    }

    def delete_session(db: Solr.Database, session_name: String): Unit =
      db.transaction {
        val delete = read_domain(db, Solr.filter(Fields.session, Solr.phrase(session_name)))
        if (delete.nonEmpty) db.execute_batch_delete(delete.toList)
      }

    def query_stats(db: Solr.Database, q: Solr.Source, fq: List[Solr.Source]): Stats =
      db.execute_stats_query(
        List(Fields.session_facet, Fields.theory_facet, Fields.command, Fields.consts_facet,
          Fields.typs_facet, Fields.thms_facet, Fields.start_line),
        { res =>
          val results = res.count
          val sessions = res.count(Fields.session_facet)
          val theories = res.count(Fields.theory_facet)
          val commands = res.count(Fields.theory_facet)
          val consts = res.count(Fields.consts_facet)
          val typs = res.count(Fields.typs_facet)
          val thms = res.count(Fields.thms_facet)

          Stats(results, sessions, theories, commands, consts, typs, thms)
        }, q = q, fq = fq)

    def query_facets(db: Solr.Database, q: Solr.Source, fq: List[Solr.Source]): Facets =
      db.execute_facet_query(
        List(Fields.chapter, Fields.session_facet, Fields.theory_facet, Fields.command, Fields.kinds,
          Fields.consts_facet, Fields.typs_facet, Fields.thms_facet),
        { res =>
          val chapter = res.string(Fields.chapter)
          val sessions = res.string(Fields.session_facet)
          val theories = res.string(Fields.theory_facet)
          val commands = res.string(Fields.command)
          val kinds = res.string(Fields.kinds)
          val consts = res.string(Fields.consts_facet)
          val typs = res.string(Fields.typs_facet)
          val thms = res.string(Fields.thms_facet)

          Facets(chapter, sessions, theories, commands, kinds, consts, typs, thms)
        }, q = q, fq = fq)


    /* queries */

    def solr_field(field: Field, select: Boolean = false): Solr.Field =
      field match {
        case Field.chapter => Fields.chapter
        case Field.session if select => Fields.session_facet
        case Field.session => Fields.session
        case Field.theory if select => Fields.theory_facet
        case Field.theory => Fields.theory
        case Field.command => Fields.command
        case Field.source => Fields.src
        case Field.names => Fields.names
        case Field.consts if select => Fields.consts_facet
        case Field.consts => Fields.consts
        case Field.typs if select => Fields.typs_facet
        case Field.typs => Fields.typs
        case Field.thms if select => Fields.thms_facet
        case Field.thms => Fields.thms
        case Field.kinds => Fields.kinds
      }

    def solr_query(query: Query): (Solr.Source, List[Solr.Source]) = {
      def solr_atom(atom: Atom): List[Solr.Source] =
        atom match {
          case Atom.Value(s) if s.isEmpty => Nil
          case Atom.Value(s) if !s.exists(Solr.wildcard(_)) => List(Solr.term(s))
          case Atom.Value(s) =>
            val terms = s.split("\\s+").toList.filterNot(_.isBlank)
            if (terms.isEmpty) Nil else terms.map(Solr.wildcard)
          case Atom.Exact(s) => List(Solr.phrase(s))
        }

      def solr_atoms(field: Field, atoms: List[Atom]): List[Solr.Source] =
        for {
          atom <- atoms
          source <- solr_atom(atom)
        } yield Solr.filter(solr_field(field), source)

      def solr_filter(filter: Filter): List[Solr.Source] =
        filter match {
          case Field_Filter(field, atoms) => solr_atoms(field, atoms)
          case any@Any_Filter(atoms) => any.fields.flatMap(solr_atoms(_, atoms))
        }

      def solr_select(select: Select): Solr.Source = {
        val field = solr_field(select.field, select = true)
        Solr.tag(field.name, Solr.filter(field, Solr.OR(select.values.map(Solr.phrase))))
      }

      val filter = query.filters.map(filter => Solr.OR(solr_filter(filter)))
      val exclude = query.exclude.flatMap(solr_filter).map(Solr.exclude)
      val selects = query.selects.map(solr_select)

      (Solr.AND(filter ::: exclude), selects)
    }
  }

  def open_database(): Solr.Database = Solr.open_database(Find_Facts.private_data)

  def query_block(db: Solr.Database, id: String): Option[Block] = {
    val q = Solr.filter(Find_Facts.private_data.Fields.id, Solr.phrase(id))
    Find_Facts.private_data.read_blocks(db, q, Nil).blocks.headOption
  }

  def query_blocks(db: Solr.Database, query: Query, cursor: Option[String] = None): Blocks = {
    val (q, fq) = Find_Facts.private_data.solr_query(query)
    Find_Facts.private_data.read_blocks(db, q, fq, cursor = cursor)
  }

  def query_stats(db: Solr.Database, query: Query): Stats = {
    val (q, fq) = Find_Facts.private_data.solr_query(query)
    Find_Facts.private_data.query_stats(db, q, fq)
  }

  def query_facet(db: Solr.Database, query: Query): Facets = {
    val (q, fq) = Find_Facts.private_data.solr_query(query)
    Find_Facts.private_data.query_facets(db, q, fq)
  }


  /** indexing **/

  def read_blocks(
    name: Document.Node.Name,
    browser_info_context: Browser_Info.Context,
    document_info: Document_Info,
    theory_context: Export.Theory_Context,
    chapter: String
  ): List[Block] = {
    def expand_block(block: Thy_Blocks.Block): List[Thy_Blocks.Block] =
      block match {
        case Thy_Blocks.Thy(inner) => inner.flatMap(expand_block)
        case e@Thy_Blocks.Decl(inner) =>
          val inner1 = inner.flatMap(expand_block)
          if (inner.length > 1) e :: inner1 else List(e)
        case _ => List(block)
      }

    val elements = Browser_Info.Elements(html = Browser_Info.default_elements.html - Markup.ENTITY)
    val node_context = Browser_Info.Node_Context.empty

    val theory = theory_context.theory
    val snapshot =
      Build.read_theory(theory_context).getOrElse(error("Missing snapshot for " + theory))
    val version = snapshot.version.id
    val content = XML.content(snapshot.xml_markup())

    val index = Symbol.Index(content)
    val document = Line.Document(content)

    def get_source(start: Line.Position, stop: Line.Position): String = {
      val range = Text.Range(document.offset(start).get, document.offset(stop).get)
      Symbol.decode(range.substring(document.text))
    }

    val num_lines = document.lines.length

    val thy_entities =
      for {
        entity <- Export_Theory.read_theory(theory_context).entity_iterator.toList
        if Path.explode(entity.file).canonical == name.path.canonical
      } yield entity

    val entities =
      TreeMap.from(thy_entities.groupBy(entity => index.decode(entity.range).start).toList)

    val session = theory_context.session_context.session_name

    val theory_info =
      document_info.theory_by_name(session, theory).getOrElse(error("No info for theory " + theory))

    val url_path =
      browser_info_context.theory_dir(theory_info) + browser_info_context.theory_html(theory_info)

    def read_block(range: Text.Range, command: String): Block = {
      val line_range = document.range(range)
      val start_line = line_range.start.line1

      val id = theory + "#" + range.start + ".." + range.stop

      val src_before =
        get_source(Line.Position((line_range.start.line - 5).max(0)), line_range.start)
      val src = Symbol.decode(document.get_text(range).get)
      val src_after =
        get_source(line_range.stop, Line.Position((line_range.stop.line + 5).min(num_lines)))

      val xml = snapshot.xml_markup(range, elements = elements.html)
      val html =
        HTML.output(node_context.make_html(elements, xml), hidden = true, structural = false)

      val maybe_entities = entities.range(range.start, range.stop).values.toList.flatten.distinct
      def get_entities(kind: String): List[String] =
        for {
          entity <- maybe_entities
          if entity.export_kind == kind
          if range.contains(index.decode(entity.range))
        } yield entity.name

      val typs = get_entities(Export_Theory.Kind.TYPE)
      val consts = get_entities(Export_Theory.Kind.CONST)
      val thms = get_entities(Export_Theory.Kind.THM)

      Block(id = id, version = version, chapter = chapter, session = session, theory = theory,
        file = name.path, url_path = url_path, command = command, start_line = start_line,
        src_before = src_before, src = src, src_after = src_after, xml = xml, html = html,
        consts = consts, typs = typs, thms = thms)
    }

    val thy_blocks =
      for (block <- Thy_Blocks.read_blocks(snapshot).flatMap(expand_block))
      yield read_block(block.range, block.command)

    val comment =
      Markup.Elements(Markup.COMMENT, Markup.COMMENT1, Markup.COMMENT2, Markup.COMMENT3)
    val comments =
      for (info <- snapshot.select(Text.Range.full, comment, _ => _ => Some(())))
      yield read_block(info.range, "")

    thy_blocks ::: comments
  }

  def index_blocks(
    options: Options,
    sessions: List[String],
    clean: Boolean = false,
    progress: Progress = new Progress
  ): Unit = {
    val store = Store(options)

    val selection = Sessions.Selection(sessions = sessions)
    val session_structure = Sessions.load_structure(options).selection(selection)
    val deps = Sessions.Deps.load(session_structure)
    val browser_info_context = Browser_Info.context(session_structure)

    if (clean) {
      progress.echo("Cleaning " + Solr.solr_home.implode)
      Isabelle_System.rm_tree(Solr.solr_home)
    }

    if (sessions.isEmpty) progress.echo("Nothing to index")
    else {
      val stats =
        using(open_database()) { db =>
          using(Export.open_database_context(store)) { database_context =>
            val document_info = Document_Info.read(database_context, deps, sessions)
            sessions.foreach(session =>
              using(database_context.open_session0(session)) { session_context =>
                val info = session_structure(session)
                progress.echo("Session " + info.chapter + "/" + session + " ...")

                Find_Facts.private_data.delete_session(db, session)
                deps(session).proper_session_theories.foreach { name =>
                  progress.echo("Theory " + name.theory + " ...")
                  val theory_context = session_context.theory(name.theory)
                  val blocks = read_blocks(name, browser_info_context, document_info,
                    theory_context, info.chapter)
                  Find_Facts.private_data.update_theory(db, theory_context.theory, blocks)
                }
              })
          }

          val query = Query(Field_Filter(Field.session, sessions.map(Atom.Exact(_))))
          Find_Facts.query_stats(db, query)
        }

      progress.echo("Indexed " + stats.results + " blocks with " +
        stats.consts + " consts, " + stats.typs + " typs, " + stats.thms + " thms")
    }
  }


  /* Isabelle tool wrapper */

  val isabelle_tool = Isabelle_Tool("find_facts_index", "index sessions for find_facts",
    Scala_Project.here,
    { args =>
      var clean = false
      var options = Options.init()

      val getopts = Getopts("""
  Usage: isabelle find_facts_index [OPTIONS] [SESSIONS ...]

    Options are:
      -c           clean previous index
      -o OPTION    override Isabelle system OPTION (via NAME=VAL or NAME)

    Index sessions for find_facts.
  """,
        "c" -> (_ => clean = true),
        "o:" -> (arg => options = options + arg))

      val sessions = getopts(args)

      val progress = new Console_Progress()

      index_blocks(options, sessions, clean = clean, progress = progress)
    })


  /** querying **/

  /* requests and parsing */

  case class Query_Blocks(query: Query, cursor: String)

  object Parse {
    def atom(json: JSON.T): Option[Atom] =
      JSON.string(json, "value").map(Atom.Value(_)) orElse
        JSON.string(json, "exact").map(Atom.Exact(_))

    def field(name: String): Option[Field] = Field.values.find(_.toString == name)

    def filter(json: JSON.T): Option[Filter] =
      for {
        atoms <- JSON.list(json, "either", atom)
        filter <-
          JSON.string(json, "field") match {
            case None => Some(Any_Filter(atoms))
            case Some(name) => for (field <- field(name)) yield Field_Filter(field, atoms)
          }
      } yield filter

    def select(json: JSON.T): Option[Select] =
      for {
        name <- JSON.string(json, "field")
        field <- field(name)
        values <- JSON.strings(json, "values")
      } yield Select(field, values)

    def query(json: JSON.T): Option[Query] =
      for {
        filters <- JSON.list(json, "filters", filter)
        exclude <- JSON.list(json, "exclude", filter)
        selects <- JSON.list(json, "selects", select)
      } yield Query(filters, exclude, selects)

    def query_blocks(json: JSON.T): Option[Query_Blocks] =
      for {
        query <- JSON.value(json, "query", query)
        cursor <- JSON.string(json, "cursor")
      } yield Query_Blocks(query, cursor)

    def query_block(json: JSON.T): Option[String] = for (id <- JSON.string(json, "id")) yield id
  }


  /* responses and encoding */

  case class Result(blocks: Blocks, facets: Facets)

  class Encode(options: Options) {
    val library_base_url = Url(options.string("browser_info_url_library"))
    val afp_base_url = Url(options.string("browser_info_url_afp"))

    def url(block: Block): Url = {
      val base_url = if (block.chapter == AFP.chapter) afp_base_url else library_base_url
      base_url.resolve(block.url_path.implode)
    }

    def block(block: Block): JSON.T =
      JSON.Object(
        "id" -> block.id,
        "chapter" -> block.chapter,
        "session" -> block.session,
        "theory" -> block.theory,
        "url" -> url(block).toString,
        "command" -> block.command,
        "start_line" -> block.start_line,
        "src_before" -> block.src_before,
        "src_after" -> block.src_after,
        "html" -> block.html,
        "consts" -> block.consts,
        "typs" -> block.typs,
        "thms" -> block.thms)

    def blocks(blocks: Blocks): JSON.T =
      JSON.Object(
        "num_found" -> blocks.num_found,
        "blocks" -> blocks.blocks.map(block),
        "cursor" -> blocks.cursor)

    def facets(facet: Facets): JSON.T =
      JSON.Object(
        "chapter" -> facet.chapter,
        "session" -> facet.session,
        "theory" -> facet.theory,
        "command" -> facet.command,
        "kinds" -> facet.kinds,
        "consts" -> facet.consts,
        "typs" -> facet.typs,
        "thms" -> facet.thms)

    def result(result: Result): JSON.T =
      JSON.Object(
        "blocks" -> blocks(result.blocks),
        "facets" -> facets(result.facets))
  }


  /* find facts */

  abstract class REST_Service(path: Path, progress: Progress, method: String = "POST")
    extends HTTP.Service(path.implode, method = method) {
    def handle(body: JSON.T): Option[JSON.T]

    def apply(request: HTTP.Request): Option[HTTP.Response] =
      try {
        for {
          json <-
            Exn.capture(JSON.parse(request.input.text)) match {
              case Exn.Res(json) => Some(json)
              case _ =>
                progress.echo("Could not parse: " + quote(request.input.text), verbose = true)
                None
            }
          res <-
            handle(json) match {
              case Some(res) => Some(res)
              case None =>
                progress.echo("Invalid request: " + JSON.Format(json), verbose = true)
                None
            }
        } yield HTTP.Response(Bytes(JSON.Format(res)), content_type = "application/json")
      }
      catch { case exn: Throwable =>
        val uuid = UUID.random()
        progress.echo_error_message("Server error <" + uuid + ">: " + exn)
        Some(HTTP.Response.text("internal server error: " + uuid))
      }
  }

  def find_facts(
    options: Options,
    port: Int,
    devel: Boolean = false,
    progress: Progress = new Progress
  ): Unit = {
    val encode = new Encode(options)
    val logo = Bytes.read(Path.explode("$FIND_FACTS_HOME/web/favicon.ico"))

    val isabelle_style = HTML.fonts_css("/fonts/" + _) + "\n\n" + File.read(HTML.isabelle_css)

    val project = Elm.Project("Find_Facts", Path.explode("$FIND_FACTS_HOME/web"), head = List(
      HTML.style("html,body {width: 100%, height: 100%}"),
      Web_App.More_HTML.icon("data:image/x-icon;base64," + logo.encode_base64.text),
      HTML.style_file("isabelle.css"),
      HTML.style_file("https://fonts.googleapis.com/css?family=Roboto:300,400,500|Material+Icons"),
      HTML.style_file(
        "https://unpkg.com/material-components-web-elm@9.1.0/dist/material-components-web-elm.min.css"),
      HTML.script_file(
        "https://unpkg.com/material-components-web-elm@9.1.0/dist/material-components-web-elm.min.js")))

    val frontend = project.build_html(progress)

    using(Solr.open_database(Find_Facts.private_data)) { db =>
      val stats = Find_Facts.query_stats(db, Query(Nil))
      progress.echo("Started find facts with " + stats.results + " blocks, " +
        stats.consts + " consts, " + stats.typs + " typs, " + stats.thms + " thms")

      val server =
        HTTP.server(port, name = "", services = List(
          HTTP.Fonts_Service,
          new HTTP.Service("isabelle.css") {
            def apply(request: HTTP.Request): Option[HTTP.Response] =
              Some(HTTP.Response(Bytes(isabelle_style), "text/css"))
          },
          new HTTP.Service("app") {
            def apply(request: HTTP.Request): Option[HTTP.Response] =
              Some(HTTP.Response.html(if (devel) project.build_html(progress) else frontend))
          },
          new REST_Service(Path.explode("api/block"), progress) {
            def handle(body: JSON.T): Option[JSON.T] =
              for {
                request <- Parse.query_block(body)
                block <- query_block(db, request)
              } yield encode.block(block)
          },
          new REST_Service(Path.explode("api/blocks"), progress) {
            def handle(body: JSON.T): Option[JSON.T] =
              for (request <- Parse.query_blocks(body))
              yield encode.blocks(query_blocks(db, request.query, Some(request.cursor)))
          },
          new REST_Service(Path.explode("api/query"), progress) {
            def handle(body: JSON.T): Option[JSON.T] =
              for (query <- Parse.query(body)) yield {
                val facet = query_facet(db, query)
                val blocks = query_blocks(db, query)
                encode.result(Result(blocks, facet))
              }
          }))

      server.start()
      progress.echo("Server started on port " + server.http_server.getAddress.getPort)

      @tailrec
      def loop(): Unit = {
        Thread.sleep(Long.MaxValue)
        loop()
      }

      Isabelle_Thread.interrupt_handler(_ => server.stop()) { loop() }
    }
  }


  /* Isabelle tool wrapper */

  val isabelle_tool1 = Isabelle_Tool("find_facts", "run find_facts server", Scala_Project.here,
  { args =>
    var devel = false
    var options = Options.init()
    var port = 8080
    var verbose = false

    val getopts = Getopts("""
Usage: isabelle find_facts [OPTIONS]

  Options are:
    -d           devel mode
    -o OPTION    override Isabelle system OPTION (via NAME=VAL or NAME)
    -p PORT      explicit web server port
    -v           verbose server

  Run a find_facts query.
""",
        "d" -> (_ => devel = true),
        "o:" -> (arg => options = options + arg),
        "p:" -> (arg => port = Value.Int.parse(arg)),
        "v" -> (_ => verbose = true))

    val more_args = getopts(args)
    if (more_args.nonEmpty) getopts.usage()

    val progress = new Console_Progress(verbose = verbose)

    find_facts(options, port, devel = devel, progress = progress)
  })
}
