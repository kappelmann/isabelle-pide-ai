/*  Title:      AI/src/Server/pide_ai_tool_endpoint.scala
    Author:     Kevin Kappelmann

Running tool calls once the server is ready.
*/

package isabelle.pide.ai

import isabelle._

abstract class PIDE_AI_Tool_Endpoint(
  server: PIDE_AI_Server,
  connection: Line_Connection,
  context: PIDE_AI_Client_Context,
  log: Logger
) extends PIDE_AI_Endpoint(server, connection, context) {
  def tool_not_found(id: String): String = s"Tool not found: ${quote(id)}"

  val tool_call_scope = new Task_Scope[PIDE_AI_Tool_Call]

  def start_tool_call(
    tool: PIDE_AI_Tool,
    tool_call: MCP.Tool_Call,
    report_progress: (MCP.Progress_Token, Long, JSON.T) => Unit,
    report_result: PIDE_AI_Tool_Result => Unit
  ): PIDE_AI_Tool_Call = {
    val serial = Counter.make()
    def report(json: JSON.T): Unit =
      for (token <- tool_call.progress_token) report_progress(token, serial(), json)
    val progress = new Reporting_Progress(
      new Sub_Progress(server.base_progress) with Progress.Local_Interrupts,
      report_output = _.foreach(message => report(message.message.output_text)),
      report_json = report)
    val call = new PIDE_AI_Tool_Call(server, tool, tool_call, progress, log, context)
    tool_call_scope.fork("pide_ai_tool_call", call, on_finish = { result =>
      progress.stop_reporting()
      for (tool_result <- Exn.the_res.lift(result)) {
        PIDE_AI_Util.log_failure(log,
          s"Failed to deliver result of tool ${quote(tool.id)} to client ${connection.remote}",
          Exn.capture { report_result(tool_result) })
      }
    })
    call
  }

  protected def serve(): Unit

  final def run(): Unit =
    PIDE_AI_Util.release_combined(List(
      Exn.capture { if (Exn.is_res(server.ready.join_result)) serve() },
      Exn.capture { if (tool_call_scope.tasks.nonEmpty) log("Stopping tool calls...") },
      Exn.capture { tool_call_scope.stop() },
      Exn.capture { tool_call_scope.join() }))
}
