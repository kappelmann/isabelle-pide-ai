/*  Title:      AI/src/Tools/pide_ai_tool.scala
    Author:     Maximilian Schäffeler, Kevin Kappelmann
*/

package isabelle.pide.ai

import isabelle._

object PIDE_AI_Tool extends PIDE_AI_Registry.Kind[PIDE_AI_Tool, PIDE_AI_Tools] {
  val kind: String = "tool"
  val include_option: String = "pide_ai_tools_include"
  val exclude_option: String = "pide_ai_tools_exclude"
  def services: Class[PIDE_AI_Tools] = classOf[PIDE_AI_Tools]
  def id(tool: PIDE_AI_Tool): String = tool.id

  trait Read_Only extends PIDE_AI_Tool {
    override def annotations: Option[JSON.Object.T] = Some(JSON_Object("readOnlyHint" -> true))
  }

  trait Destructive extends PIDE_AI_Tool {
    override def annotations: Option[JSON.Object.T] = Some(JSON_Object("destructiveHint" -> true))
  }
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
    progress: PIDE_AI_Progress,
    context: PIDE_AI_Client_Context
  ): PIDE_AI_Tool_Result
}

class PIDE_AI_Tools(tools: PIDE_AI_Tool*) extends PIDE_AI_Registry.Entries(tools.toList)
