/*  Title:      MCP/src/pide_mcp_tool.scala
    Author:     Maximilian Schäffeler, Kevin Kappelmann
*/

package isabelle.pide.mcp

import isabelle._

object PIDE_MCP_Tool {
  val kind: String = "tool"

  def include_ids(options: Options): String = options.string("pide_mcp_tools_include")
  def exclude_ids(options: Options): String = options.string("pide_mcp_tools_exclude")

  def make_table(
    include_ids: String = PIDE_AI_Registry.all_option,
    exclude_ids: String = ""
  ): Map[String, PIDE_MCP_Tool] =
    PIDE_AI_Registry.make_table(kind,
      Isabelle_System.make_services(classOf[PIDE_MCP_Tools]).flatMap(_.entries), _.id,
      include_ids, exclude_ids)
}

abstract class PIDE_MCP_Tool(id: String) extends PIDE_MCP_Service(id) {
  override def kind: String = PIDE_MCP_Tool.kind

  def input_schema: JSON.Object.T
  def annotations: Option[JSON.Object.T] = None

  def check_args(args: JSON.Object.T): Unit = JSON_Schema.check_properties(input_schema, args)

  override def json: JSON.Object.T =
    super.json ++ JSON_Object("inputSchema" -> input_schema) ++
    JSON.optional("annotations" -> annotations)

  def handle(
    server: PIDE_AI_Server,
    args: JSON.Object.T,
    progress: Progress
  ): PIDE_MCP_Tool_Result
}

class PIDE_MCP_Tools(tools: PIDE_MCP_Tool*) extends Isabelle_System.Service {
  def entries: List[PIDE_MCP_Tool] = tools.toList
}
