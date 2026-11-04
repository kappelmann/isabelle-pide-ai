/*  Title:      AI/src/Tools/pide_ai_tool.scala
    Author:     Maximilian Schäffeler, Kevin Kappelmann
*/

package isabelle.pide.ai

import isabelle._

object PIDE_AI_Tool {
  val kind: String = "tool"

  def include_ids(options: Options): String = options.string("pide_ai_tools_include")
  def exclude_ids(options: Options): String = options.string("pide_ai_tools_exclude")

  def make_table(
    include_ids: String = PIDE_AI_Registry.all_option,
    exclude_ids: String = ""
  ): Map[String, PIDE_AI_Tool] =
    PIDE_AI_Registry.make_table(kind,
      Isabelle_System.make_services(classOf[PIDE_AI_Tools]).flatMap(_.entries), _.id,
      include_ids, exclude_ids)
}

abstract class PIDE_AI_Tool(id: String) extends PIDE_AI_Service(id) {
  override def kind: String = PIDE_AI_Tool.kind

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
  ): PIDE_AI_Tool_Result
}

class PIDE_AI_Tools(tools: PIDE_AI_Tool*) extends Isabelle_System.Service {
  def entries: List[PIDE_AI_Tool] = tools.toList
}
