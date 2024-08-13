/*  Author:     Fabian Huch, TU Muenchen

Support for full-text search via Solr. See also: https://solr.apache.org/
*/

package isabelle


import scala.jdk.CollectionConverters.*

import org.apache.solr.client.solrj.embedded.EmbeddedSolrServer
import org.apache.solr.client.solrj.request.json.{JsonQueryRequest, TermsFacetMap}
import org.apache.solr.client.solrj.response.json.{BucketJsonFacet, NestableJsonFacet}
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

  val all: Source = "*"

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

  def exclude(from: Source, arg: Source): Source = from + if_proper(arg, " -" + arg)

  val query_all: Source = "*:" + all


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
    val int = Type("int", "IntPointField")
    val long = Type("long", "LongPointField")
    val float = Type("float", "FloatPointField")
    val double = Type("double", "DoublePointField")
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
    object float {
      def update(field: Field, x: Float): Unit = rep.addField(field.name, x)
      def update(field: Field, x: Option[Float]): Unit = rep.addField(field.name, x.orNull)
      def update(field: Field, x: List[Float]): Unit = rep.addField(field.name, x.toArray)
    }
    object double {
      def update(field: Field, x: Double): Unit = rep.addField(field.name, x)
      def update(field: Field, x: Option[Double]): Unit = rep.addField(field.name, x.orNull)
      def update(field: Field, x: List[Double]): Unit = rep.addField(field.name, x.toArray)
    }
    object string {
      def update(field: Field, x: String): Unit = rep.addField(field.name, x)
      def update(field: Field, x: Option[String]): Unit = rep.addField(field.name, x.orNull)
      def update(field: Field, x: List[String]): Unit = rep.addField(field.name, x.toArray)
    }
  }


  /* results */

  class Result private[Solr](rep: SolrDocument) {
    private def single[A](field: Field): A = {
      val elem = rep.getFieldValue(field.name)
      if (elem == null) error("No such field: " + field.name) else elem.asInstanceOf[A]
    }
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

    def float(field: Field): Float = single(field)
    def get_float(field: Field): Option[Float] = option(field)
    def list_float(field: Field): List[Float] = list(field)

    def double(field: Field): Double = single(field)
    def get_double(field: Field): Option[Double] = option(field)
    def list_double(field: Field): List[Double] = list(field)

    def string(field: Field): String = single(field)
    def get_string(field: Field): Option[String] = option(field)
    def list_string(field: Field): List[String] = list(field)
  }

  class Results private[Solr](
    solr: EmbeddedSolrServer,
    query: SolrQuery,
    private var cursor: String
  ) extends Iterator[Result] {
    private def response: QueryResponse =
      solr.query(query.set(CursorMarkParams.CURSOR_MARK_PARAM, cursor))

    private var _response = response
    private var _iterator = _response.getResults.iterator

    def num_found: Long = _response.getResults.getNumFound
    def next_cursor: String = _response.getNextCursorMark

    def hasNext: Boolean = _iterator.hasNext
    def next(): Result = {
      val res = new Result(_iterator.next())

      if (!_iterator.hasNext && next_cursor != cursor) {
        cursor = next_cursor
        _response = response
        _iterator = _response.getResults.iterator
      }

      res
    }
  }


  /* facet results */

  class Facet_Result private[Solr](rep: NestableJsonFacet) {
    def count: Long = rep.getCount

    private def get_bucket[A](bucket: BucketJsonFacet): (A, Long) =
      bucket.getVal.asInstanceOf[A] -> bucket.getCount
    private def get_facet[A](field: Field): Map[A, Long] =
      rep.getBucketBasedFacets(field.name).getBuckets.asScala.map(get_bucket).toMap

    def bool(field: Field): Map[Boolean, Long] = get_facet(field)
    def int(field: Field): Map[Int, Long] = get_facet(field)
    def long(field: Field): Map[Long, Long] = get_facet(field)
    def string(field: Field): Map[String, Long] = get_facet(field)
  }


  /* stat results */

  private def count_field(field: Field): String = field.name + "/count"

  class Stat_Result private[Solr](rep: NestableJsonFacet) {
    def count: Long = rep.getCount
    def count(field: Field): Long = rep.getStatValue(count_field(field)).asInstanceOf[Long]
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

    def execute_query[A](
      id: Field,
      fields: List[Field],
      q: Source,
      cursor: Option[String],
      chunk_size: Int,
      make_result: Results => A,
    ): A = {
      val query = new SolrQuery(q)
        .setFields(fields.map(_.name): _*)
        .setRows(chunk_size)
        .addSort("score", SolrQuery.ORDER.desc)
        .addSort(id.name, SolrQuery.ORDER.asc)

      val cursor1 = cursor.getOrElse(CursorMarkParams.CURSOR_MARK_START)
      make_result(new Results(solr, query, cursor1))
    }

    def transaction[A](body: => A): A =
      try {
        val result = body
        solr.commit()
        result
      }
      catch { case exn: Throwable => solr.rollback(); throw exn }

    def execute_facet_query[A](
      fields: List[Field],
      q: Source,
      make_result: Facet_Result => A,
      max_terms: Int = -1
    ): A = {
      val query = new JsonQueryRequest().setQuery(q).setLimit(0)

      for (field <- fields)
        query.withFacet(field.name, new TermsFacetMap(field.name).setLimit(max_terms))

      make_result(new Facet_Result(query.process(solr).getJsonFacetingResponse))
    }

    def execute_stats_query[A](
      fields: List[Field],
      q: Source,
      make_result: Stat_Result => A
    ): A = {
      val query = new JsonQueryRequest().setQuery(q).setLimit(0)

      for (field <- fields) query.withStatFacet(count_field(field), "unique(" + field.name + ")")

      make_result(new Stat_Result(query.process(solr).getJsonFacetingResponse))
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