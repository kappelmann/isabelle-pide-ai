/*  Title:      AI/src/General/pide_ai_registry.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.ai

import isabelle._

object PIDE_AI_Registry {
  val all_option: String = "*"

  def make_table[A](
    kind: String,
    entries: List[A],
    id: A => String,
    include_ids: String,
    exclude_ids: String
  ): Map[String, A] = {
    val table = entries.foldLeft(Map.empty[String, A]) {
      case (table, entry) =>
        if (table.isDefinedAt(id(entry)))
          error(s"Duplicate $kind: ${Library.quote(id(entry))}")
        else table + (id(entry) -> entry)
    }
    def select(ids: String): Set[String] =
      if (ids == all_option) table.keySet
      else {
        val list = space_explode(',', ids)
        val unknown = list.filterNot(table.isDefinedAt)
        if (unknown.nonEmpty)
          error(s"Unknown $kind(s): ${commas_quote(unknown)}. " +
            s"Available $kind(s): ${commas_quote(table.keys.toList.sorted)}")
        list.toSet
      }
    val included = select(include_ids)
    val excluded = select(exclude_ids)
    table.filter { (id, _) => included(id) && !excluded(id) }
  }

  abstract class Entries[A](val entries: List[A]) extends Isabelle_System.Service

  trait Kind[A, S <: Entries[A]] {
    def kind: String
    def include_option: String
    def exclude_option: String
    def services: Class[S]
    def id(entry: A): String

    def include_ids(options: Options): String = options.string(include_option)
    def exclude_ids(options: Options): String = options.string(exclude_option)

    def make_table(include_ids: String = all_option, exclude_ids: String = ""): Map[String, A] =
      PIDE_AI_Registry.make_table(kind,
        Isabelle_System.make_services(services).flatMap(_.entries), id, include_ids, exclude_ids)
  }
}
