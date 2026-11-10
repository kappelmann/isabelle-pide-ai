/*  Title:      AI/src/PIDE/pide_ai_origin.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.ai

import isabelle._
import isabelle.symbols.Symbols

import scala.collection.mutable
import scala.collection.immutable.VectorMap

object PIDE_AI_Origin {
  def def_label(name_space_entry: Name_Space.Entry, unicode_symbols: Boolean): String =
    if (Long_Name.qualifier(name_space_entry.def_label) == Markup.COMMAND) ""
    else Symbols.output_decoded(unicode_symbols, name_space_entry.def_label)

  def unresolved(
    name_space_entry: Name_Space.Entry,
    name: String,
    source_name: String,
    line: Int,
    msg: String,
    unicode_symbols: Boolean
  ): PIDE_AI_Origin = PIDE_AI_Origin(name, name_space_entry.kind,
    def_label(name_space_entry, unicode_symbols), location = Some(Left(source_name)),
    line = Some(line),
    note = Some(s"The origin's source file ${quote(source_name)} " +
      s"could not be resolved: $msg"))

  class Context(
    val session: PIDE_AI_Session,
    val snapshot: Document.Snapshot,
    snippet_lines: Int,
    filter_node_names: Option[Set[Document.Node.Name]],
    origin_not_loaded: String
  ) {
    private val cache = mutable.Map.empty[Document.Node.Name, Line.Document]
    def doc(node_name: Document.Node.Name): Line.Document =
      cache.getOrElseUpdate(node_name, Line.Document(session.node_source(snapshot, node_name)))

    def resolved(
      name_space_entry: Name_Space.Entry,
      name: String,
      node_name: Document.Node.Name,
      line: Int
    ): PIDE_AI_Origin = PIDE_AI_Origin(name, name_space_entry.kind,
      def_label(name_space_entry, session.resources.unicode_symbols),
      location = Some(Right(node_name)),
      line = Some(line), snippet = Option.when(snippet_lines > 0)(
        PIDE_AI_Util.numbered_lines_range(doc(node_name), line, line + snippet_lines - 1)))

    def resolve(name_space_entry: Name_Space.Entry, name: String): Option[PIDE_AI_Origin] = {
      def resolve_at(source_name: String, line: Int): Option[PIDE_AI_Origin] =
        try {
          val node_name = session.resources.source_node_name(session.store, source_name)
          Option.when(filter_node_names.forall(_.contains(node_name)))(
            resolved(name_space_entry, name, node_name, line))
        }
        catch {
          case ERROR(msg) => Some(unresolved(name_space_entry, name, source_name, line, msg,
            session.resources.unicode_symbols))
        }
      name_space_entry.properties match {
        case Position.Item_Def_File(def_file, def_line, _) => resolve_at(def_file, def_line)
        case Position.Item_Def_Id(def_id, def_range) =>
          snapshot.find_command_position(def_id, def_range.start) match {
            case Some(pos) => resolve_at(pos.name, pos.line1)
            case None => Some(PIDE_AI_Origin(name, name_space_entry.kind,
              def_label(name_space_entry, session.resources.unicode_symbols),
              note = Some(origin_not_loaded)))
          }
        case _ => None
      }
    }
  }

  def origins(
    session: PIDE_AI_Session,
    snapshot: Document.Snapshot,
    range: Option[Text.Range],
    snippet_lines: Int,
    kinds: List[String],
    filter_node_names: Option[Set[Document.Node.Name]],
    origin_not_loaded: String
  ): List[PIDE_AI_Origin] = {
    val restricted = PIDE_AI_Util.restrict_text_range(snapshot.node.source, range)
    val context =
      new Context(session, snapshot, snippet_lines, filter_node_names, origin_not_loaded)
    snapshot.cumulate(restricted, List.empty[PIDE_AI_Origin],
      Markup.Elements(Markup.ENTITY), _ => {
      case (origins, Text.Info(r, XML.Elem(Markup.Entity(name_space_entry), _)))
      if kinds.contains(name_space_entry.kind) =>
        val name = PIDE_AI_Util.display_name(Some(name_space_entry), r, snapshot.node.source,
          session.resources.unicode_symbols)
        context.resolve(name_space_entry, name).map(_ :: origins)
      case _ => None
    }).flatMap(_.info)
  }

  def origins_json(
    resources: PIDE_AI_Resources,
    origins: List[PIDE_AI_Origin],
    cwd: Option[Path]
  ): Map[String, List[JSON.Object.T]] = {
    val grouped = origins.foldLeft(VectorMap.empty[String, List[PIDE_AI_Origin]]) {
      case (groups, origin) =>
        val node = origin.location match {
          case Some(Right(node_name)) => resources.print_node_name(node_name, cwd)
          case Some(Left(source_name)) => PIDE_AI_Util.print_file(source_name, cwd)
          case None => ""
        }
        groups.updated(node, origin :: groups.getOrElse(node, Nil))
    }
    grouped.map { (node, entries) =>
      val sites = entries.sortBy(entry => (entry.line, entry.name, entry.kind))
        .foldRight(List.empty[List[PIDE_AI_Origin]]) {
          case (entry, (site @ next :: rest) :: sites) if next.line == entry.line =>
            (entry :: (if (next.name == entry.name) rest else site)) :: sites
          case (entry, sites) => List(entry) :: sites
        }
      node -> sites.map(site =>
        JSON_Object.flatten(
          site.head.line.map("line" -> _),
          site.head.snippet.map("snippet" -> _)) +
        ("entities" -> site.map(_.json)))
    }
  }
}

sealed case class PIDE_AI_Origin(
  name: String,
  kind: String,
  def_label: String,
  location: Option[Either[String, Document.Node.Name]] = None,
  line: Option[Int] = None,
  snippet: Option[String] = None,
  note: Option[String] = None
) {
  def json: JSON.Object.T =
    JSON_Object.flatten(
      Some("name" -> name),
      Some("kind" -> kind),
      Option.when(def_label.nonEmpty)("def_label" -> def_label),
      note.map("note" -> _))
}
