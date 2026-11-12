/*  Title:      AI/src/Tools/read_tool.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.ai

import isabelle._

class Read_Tool extends PIDE_AI_Tool("read") {
  def description: String =
    "Read line-numbered content. " + PIDE_AI_Tool_Schema.range_perspective + " " +
    PIDE_AI_Tool_Schema.implicit_load_files

  def input_schema: JSON.Object.T = JSON_Schema.object_schema(
    List(PIDE_AI_Tool_Schema.running_session_arg, PIDE_AI_Tool_Schema.node_arg,
      PIDE_AI_Tool_Schema.start_line_default_arg, PIDE_AI_Tool_Schema.opt_end_line_arg,
      PIDE_AI_Tool_Schema.disk_sync_arg))

  def handle(
    server: PIDE_AI_Server,
    args: JSON.Object.T,
    progress: PIDE_AI_Progress,
    context: PIDE_AI_Client_Context
  ): PIDE_AI_Tool_Result =
    PIDE_AI_Tool_Result.result {
      val session = PIDE_AI_Tool_Util.running_session_param(server.sessions, args)
      val node_name = PIDE_AI_Tool_Util.node_param(session, args, context.cwd)
      val start_line = PIDE_AI_Tool_Schema.start_line_default_arg.get(args)
      val opt_end_line = PIDE_AI_Tool_Schema.opt_end_line_arg.get(args)
      PIDE_AI_Util.check_ordered_lines(start_line, opt_end_line)
      val sync = PIDE_AI_Tool_Schema.disk_sync_arg.get(args)
      if (sync == PIDE_AI_Tool_Util.Sync.node_and_ancestors)
        PIDE_AI_Tool_Util.stable_synced(session, List(node_name), sync, progress)
      val text = session.read_update_resolve(node_name, List((start_line, opt_end_line)),
        await_stable_before_resolve = true, hide_others = true, progress = progress,
        sync = sync == PIDE_AI_Tool_Util.Sync.node)
      val doc = Line.Document(text)
      val (_, end_line) = PIDE_AI_Util.check_lines(start_line, opt_end_line, doc.lines.length)
      PIDE_AI_Util.numbered_lines_range(doc, start_line, end_line)
    }
}

class Read_Tools extends PIDE_AI_Tools(new Read_Tool)
