/*  Title:      AI/src/Tools/edit_get_state_tool.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.ai

import isabelle._

class Edit_Get_State_Tool extends PIDE_AI_Tool("edit_get_state") {
  def description: String =
    Edit_Tool.base_description + " " + Get_State_Tool.base_description("edited commands") +" " +
    PIDE_AI_Tool_Schema.range_perspective + " " + PIDE_AI_Tool_Schema.implicit_reload_file

  val timeout_arg = Get_State_Tool.Request.timeout_arg(5)

  def input_schema: JSON.Object.T = JSON_Schema.object_schema(
    Edit_Tool.edit_args ::: Get_State_Tool.Request.args(timeout_arg))

  override def annotations: Option[JSON.Object.T] = Some(JSON_Object("destructiveHint" -> true))

  def handle(
    server: PIDE_AI_Server,
    args: JSON.Object.T,
    progress: Progress
  ): PIDE_AI_Tool_Result =
    PIDE_AI_Tool_Result.result {
      val session = PIDE_AI_Tool_Util.running_session_param(server.sessions, args)
      val node_name = PIDE_AI_Tool_Util.origin_param(session, args)
      val (edited_lines, json) = Edit_Tool.read_update_edit(session, node_name, args, progress)
      json ++ JSON.optional("state" -> PIDE_AI_Util.line_ranges_span(edited_lines).map {
        case (start_line, end_line) => Get_State_Tool.state(session, node_name, start_line,
          Some(end_line), Get_State_Tool.Request.from_json(args, timeout_arg), progress)
      })
    }
}

class Edit_Get_State_Tools extends PIDE_AI_Tools(new Edit_Get_State_Tool)
