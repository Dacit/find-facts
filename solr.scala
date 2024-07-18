/*  Author:     Fabian Huch, TU Muenchen

Support for full-text search via Solr. See also: https://solr.apache.org/
*/

package isabelle


import scala.jdk.CollectionConverters.*

import org.apache.solr.client.solrj.embedded.EmbeddedSolrServer
import org.apache.solr.client.solrj.response.{FacetField, QueryResponse}
import org.apache.solr.client.solrj.{SolrClient, SolrQuery}
import org.apache.solr.common.params.{CursorMarkParams, SolrParams}
import org.apache.solr.common.{SolrDocument, SolrInputDocument}


object Solr {
  val solr_home = Path.explode("$ISABELLE_HOME_USER/solr")
  
  /** query language */

  val special =
    Set("+", "-", "&&", "||", "!", "(", ")", "{", "}", "[", "]", "^", "\"", "~", "*", "?", ":", "/")

  type Source = String

  val any: Source = "*:*"

  def enclose(s: Source): Source = "(" + s + ")"

  def escape(s: String, seqs: Set[String]): Source =
    seqs.foldLeft(s)((s, seq) => s.replace(seq, "\\" + seq))

  def term(b: Boolean): Source = b.toString
  def term(i: Int): Source = i.toString
  def term(s: String): Source = "(\\s+)".r.replaceAllIn(escape(s, special), ws => "\\" + ws.source)
  def range(from: Int, to: Int): Source = "[" + from + " TO " + to + "]"
  def phrase(s: String): Source = quote(escape(s, special))
  def wildcard(s: String): Source =
    if (!s.toList.exists(Symbol.is_ascii_blank)) escape(s, special - "*" - "?")
    else error("Invalid whitespace character in wildcard: " + quote(s))

  def filter(field: Field, x: Source): Source = field.name + ":" + x

  def infix(op: Source, args: Iterable[Source]): Source = {
    val body = args.iterator.filter(_.nonEmpty).mkString(" " + op + " ")
    if_proper(body, enclose(body))
  }

  def AND(args: Iterable[Source]): Source = infix("AND", args)
  def OR(args: Iterable[Source]): Source = infix("OR", args)

  def and(args: Source*): Source = AND(args)
  def or(args: Source*): Source = OR(args)
  def not(arg: Source): Source = "NOT " + enclose(arg)


  /** solr schema **/

  object Class {
    def apply(
      markup: String,
      name: String,
      props: Properties.T = Nil,
      body: XML.Body = Nil
    ): XML.Elem = XML.Elem(Markup(markup, ("class" -> ("solr." + name)) :: props), body)
  }


  /* properties */

  val Indexed = new Properties.Boolean("indexed")
  val Stored = new Properties.Boolean("stored")
  val Column_Wise = new Properties.Boolean("docValues")
  val Multi_Valued = new Properties.Boolean("multiValued")


  /* types */

  object Type {
    val boolean = Type("boolean", "BoolField")
    val int = Type("int", "IntPointField", Column_Wise(true))
    val long = Type("long", "LongPointField", Column_Wise(true))
    val string = Type("string", "StrField")
  }

  case class Type(name: String, cls: String, props: Properties.T = Nil, body: XML.Body = Nil) {
    def defn: XML.Elem = Class("fieldType", cls, Markup.Name(name) ::: props, body)
  }


  /* fields */

  sealed case class Field(
    name: String,
    T: Type,
    props: Properties.T = Nil,
    unique_key: Boolean = false
  ) {
    def make_unique_key: Field = copy(unique_key = true)
    def defn: XML.Elem = XML.elem(Markup("field", ("name" -> name) :: ("type" -> T.name) :: props))
  }

  object Fields {
    def list(list: List[Field]): Fields = new Fields(list)
    def apply(args: Field*): Fields = list(args.toList)
  }

  final class Fields private(val list: List[Field]) extends Iterable[Field] {
    override def toString: String = list.mkString("Solr.Fields(", ", ", ")")
    def iterator: Iterator[Field] = list.iterator
  }


  /* data */

  abstract class Data(val name: String) {
    def fields: Fields
    def more_config: Map[Path, String] = Map.empty

    def stored_fields: List[Field] =
      fields.toList.filter(_.props match {
        case Stored(false) => false
        case _ => true
      })

    def unique_key: Field = Library.the_single(fields.filter(_.unique_key).toList)

    def solr_config: XML.Body = List(XML.elem("config", List(
      Class("schemaFactory", "ClassicIndexSchemaFactory"),
      XML.elem("luceneMatchVersion", XML.string(Isabelle_System.getenv("SOLR_LUCENE_VERSION"))),
      Class("updateHandler", "DirectUpdateHandler2", body = List(
        XML.elem("autoCommit", List(
          XML.elem("maxDocs", XML.string("-1")),
          XML.elem("maxTime", XML.string("-1")),
          XML.elem("maxSize", XML.string("-1")))))),
      Class("requestHandler", "SearchHandler", Markup.Name("/select")))))

    def schema: XML.Body =
      List(XML.Elem(Markup("schema",
        List(
          "name" -> "isabelle",
          "version" -> Isabelle_System.getenv("SOLR_SCHEMA_VERSION"))),
        List(
          XML.elem("uniqueKey", XML.string(unique_key.name)),
          XML.elem("fields", fields.toList.map(_.defn)),
          XML.elem("types", fields.map(_.T).toList.distinct.map(_.defn)))))
  }


  /** solr operations */

  /* documents */

  object Document {
    def empty: Document = new Document(new SolrInputDocument())
  }

  class Document private[Solr](val rep: SolrInputDocument) {
    object bool {
      def update(field: Field, x: Boolean): Unit = rep.addField(field.name, x)
      def update(field: Field, x: Option[Boolean]): Unit = rep.addField(field.name, x.orNull)
      def update(field: Field, x: List[Boolean]): Unit = rep.addField(field.name, x.toArray)
    }
    object int {
      def update(field: Field, x: Int): Unit = rep.addField(field.name, x)
      def update(field: Field, x: Option[Int]): Unit = rep.addField(field.name, x.orNull)
      def update(field: Field, x: List[Int]): Unit = rep.addField(field.name, x.toArray)
    }
    object long {
      def update(field: Field, x: Long): Unit = rep.addField(field.name, x)
      def update(field: Field, x: Option[Long]): Unit = rep.addField(field.name, x.orNull)
      def update(field: Field, x: List[Long]): Unit = rep.addField(field.name, x.toArray)
    }
    object string {
      def update(field: Field, x: String): Unit = rep.addField(field.name, x)
      def update(field: Field, x: Option[String]): Unit = rep.addField(field.name, x.orNull)
      def update(field: Field, x: List[String]): Unit = rep.addField(field.name, x.toArray)
    }
  }


  /* results */

  class Result private[Solr](rep: SolrDocument) {
    private def single[A](field: Field): A = rep.getFieldValue(field.name).asInstanceOf[A]
    private def option[A](field: Field): Option[A] = {
      val elem = rep.getFieldValue(field.name)
      if (elem == null) None else Some(elem.asInstanceOf[A])
    }
    private def list[A](field: Field): List[A] = {
      val elems = rep.getFieldValues(field.name)
      if (elems == null) Nil else elems.iterator().asScala.toList.map(_.asInstanceOf[A])
    }

    def bool(field: Field): Boolean = single(field)
    def get_bool(field: Field): Option[Boolean] = option(field)
    def list_bool(field: Field): List[Boolean] = list(field)

    def int(field: Field): Int = single(field)
    def get_int(field: Field): Option[Int] = option(field)
    def list_int(field: Field): List[Int] = list(field)

    def long(field: Field): Long = single(field)
    def get_long(field: Field): Option[Long] = option(field)
    def list_long(field: Field): List[Long] = list(field)

    def string(field: Field): String = single(field)
    def get_string(field: Field): Option[String] = option(field)
    def list_string(field: Field): List[String] = list(field)
  }

  class Facet(rep: FacetField) {
    def name = rep.getName
    def counts: Map[String, Long] =
      rep.getValues.asScala.toList.map(count => count.getName -> count.getCount).toMap
  }

  object Results {
    val chunk_size = 1000
  }

  class Results private[Solr](
    solr: EmbeddedSolrServer,
    query: SolrQuery,
    private var _cursor: String
  ) extends Iterator[Result] {
    
    def response: QueryResponse =
      if (solr.getCoreContainer.isShutDown) error("Solr database already closed")
      else solr.query(query.set(CursorMarkParams.CURSOR_MARK_PARAM, _cursor))
    
    private var _response = response
    private var _iterator = _response.getResults.iterator

    def num_found: Long = _response.getResults.getNumFound

    def hasNext: Boolean = _iterator.hasNext
    def next(): Result = {
      val res = new Result(_iterator.next())
      
      if (!_iterator.hasNext && _response.getNextCursorMark != _cursor) {
        _cursor = _response.getNextCursorMark
        _response = response
        _iterator = _response.getResults.iterator
      }
      
      res
    }
  }


  /* database */

  def open_database(data: Data, path: Path = solr_home): Database = {
    java.util.logging.LogManager.getLogManager.reset()
    File.write(Isabelle_System.make_directory(solr_home) + Path.basic("solr.xml"), "<solr/>")
    val conf_dir = solr_home + Path.make(List(data.name, "conf"))
    val server = new EmbeddedSolrServer(path.java_path, data.name)

    if (conf_dir.is_dir) server.getCoreContainer.reload(data.name)
    else {
      Isabelle_System.make_directory(conf_dir)
      File.write(conf_dir + Path.basic("schema.xml"), XML.string_of_body(data.schema))
      File.write(conf_dir + Path.basic("solrconfig.xml"), XML.string_of_body(data.solr_config))
      data.more_config.foreach((path, content) => File.write(conf_dir + path, content))
      server.getCoreContainer.create(data.name, Map.empty.asJava)
    }

    new Database(server)
  }

  class Database private[Solr](solr: EmbeddedSolrServer) extends AutoCloseable {
    override def close(): Unit = solr.close()

    def execute_query(
      id: Field,
      fields: List[Field],
      q: Source = any,
      cursor: Option[String] = None
    ): Results = {
      val query = new SolrQuery(q)
        .setFields(fields.map(_.name): _*)
        .setRows(Results.chunk_size)
        .addSort("score", SolrQuery.ORDER.desc)
        .addSort(id.name, SolrQuery.ORDER.asc)
      new Results(solr, query, cursor.getOrElse(CursorMarkParams.CURSOR_MARK_START))
    }

    def transaction[A](body: => A): A =
      try {
        val result = body
        solr.commit()
        result
      }
      catch { case exn: Throwable => solr.rollback(); throw exn }

    def execute_facet_query(
      fields: List[Field],
      q: Source = any,
      limit: Int = 100
    ): Map[Field, Facet] = {
      val query = new SolrQuery(q)
        .setFacet(true)
        .setFacetMinCount(1)
        .setFacetLimit(limit + 1)
        .setFields(fields.map(_.name): _*)
        .setRows(0)

      val result = solr.query(query)

      (for {
        field <- fields
        facet = result.getFacetField(field.name)
        if facet.getValueCount > 0 && facet.getValueCount < limit + 1
      } yield field -> Facet(facet)).toMap
    }

    def execute_batch_insert(batch: IterableOnce[Document => Unit]): Unit = {
      val it =
        batch.iterator.map { fill =>
          val doc = Document.empty
          fill(doc)
          doc.rep
        }
      solr.add(it.asJava)
    }

    def execute_batch_delete(ids: List[String]): Unit = solr.deleteById(ids.asJava)
  }
}