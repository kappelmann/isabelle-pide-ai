/*  Title:      AI/src/Tools/list_extensions_tool.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.ai

import isabelle._

object List_Extensions_Tool extends PIDE_AI_Tool("list_extensions") {
  def description: String = "List services and tools."

  val names_arg = JSON_Schema.Arg.optional(
    "names", "Extensions to list. Default: all.",
    JSON_Schema.Format.list(JSON_Schema.Format.string))
  val include_description_arg = JSON_Schema.Arg.default(
    "include_description", "", JSON_Schema.Format.bool, true)
  val include_input_schema_arg = JSON_Schema.Arg.default(
    "include_input_schema", "", JSON_Schema.Format.bool, false)

  def input_schema: JSON.Object.T = JSON_Schema.object_schema(
    List(names_arg, include_description_arg, include_input_schema_arg))

  override def annotations: Option[JSON.Object.T] = Some(JSON_Object("readOnlyHint" -> true))

  def handle(
    server: PIDE_AI_Server,
    args: JSON.Object.T,
    progress: Progress
  ): PIDE_AI_Tool_Result =
    PIDE_AI_Tool_Result.result {
      val tables = List(server.service_table, server.tool_table)
      val names = names_arg.get(args).map(_.toSet)
      for (names <- names) {
        val unknown = names.filterNot(name => tables.exists(_.isDefinedAt(name)))
        if (unknown.nonEmpty) error(s"Unknown extension(s): ${commas_quote(unknown.toList.sorted)}")
      }
      val include_description = include_description_arg.get(args)
      val include_input_schema = include_input_schema_arg.get(args)
      def entry(service: PIDE_AI_Service): JSON.T =
        if (!include_description && !include_input_schema) service.id
        else {
          val input_schema = service match {
            case tool: PIDE_AI_Tool if include_input_schema => Some(tool.input_schema)
            case _ => None
          }
          JSON_Object("name" -> service.id) ++
          JSON.optional("description" -> Option.when(include_description)(service.description)) ++
          JSON.optional("inputSchema" -> input_schema)
        }
      def entries(table: Map[String, PIDE_AI_Service]): List[JSON.T] =
        table.values.toList.filter(service => names.forall(_(service.id)))
          .sortBy(_.id).map(entry)
      JSON_Object("services" -> entries(server.service_table),
        "tools" -> entries(server.tool_table))
    }
}

class List_Extensions_Tools extends PIDE_AI_Tools(List_Extensions_Tool)
