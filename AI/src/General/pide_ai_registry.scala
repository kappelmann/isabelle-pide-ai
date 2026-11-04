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
    table.filter { case (id, _) => included(id) && !excluded(id) }
  }
}
