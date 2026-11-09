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
    args: JSON.Object.T,
    report_progress: Option[(Long, String) => Unit],
    report_result: PIDE_AI_Tool_Result => Unit
  ): PIDE_AI_Tool_Call = {
    val serial = Counter.make()
    val progress = new Reporting_Progress(
      new Sub_Progress(server.base_progress) with Progress.Local_Interrupts,
      report_output = messages =>
        for (report <- report_progress; message <- messages)
          report(serial(), message.message.output_text))
    val call = new PIDE_AI_Tool_Call(server, tool, args, progress, log, context)
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
