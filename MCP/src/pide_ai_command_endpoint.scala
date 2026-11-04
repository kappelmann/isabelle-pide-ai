/*  Title:      MCP/src/pide_ai_command_endpoint.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.mcp

import isabelle._

import java.io.IOException

object PIDE_AI_Command_Endpoint {
  val core_tools: List[PIDE_MCP_Tool] =
    List(List_Extensions_Tool, Start_Session_Tool, Stop_Server_Tool)
}

class PIDE_AI_Command_Endpoint(server: PIDE_AI_Server, connection: Line_Connection, log: Logger)
extends PIDE_AI_Tool_Endpoint(server, connection, log) {
  private def respond(body: JSON.T): Unit = connection.write_line(JSON.Format(body))

  private def respond_error(message: String): Unit =
    respond(PIDE_AI_Command.result_response(Result.Error(message)))

  private def await_client_end(): Unit =
    try { for (line <- connection.read_line()) error(s"Unexpected input after command: $line") }
    catch { case _: IOException => }

  private def handle_tool_call(tool: PIDE_MCP_Tool, args: JSON.Object.T): Unit = {
    val call = start_tool_call(tool, args,
      Some((_, message) =>
        if (!connection.closed.value) Exn.result {
          respond(PIDE_AI_Command.progress_response(message))
        }),
      { result =>
        PIDE_MCP_Util.release_combined(List(
          Exn.capture {
            if (!connection.closed.value) respond(PIDE_AI_Command.result_response(result))
          },
          Exn.capture { connection.close() }))
      })
    PIDE_MCP_Util.release_combined(List(
      Exn.capture { await_client_end() },
      Exn.capture { connection.close() },
      Exn.capture { call.stop() }))
  }

  private def handle_request(line: String): Unit =
    Exn.result { PIDE_AI_Command.parse_request(line) } match {
      case Exn.Exn(exn) => respond_error(Exn.message(exn))
      case Exn.Res((id, args)) =>
        PIDE_AI_Command_Endpoint.core_tools.find(_.id == id).orElse(server.tool(id)) match {
          case None => respond_error(tool_not_found(id))
          case Some(tool) => handle_tool_call(tool, args)
        }
    }

  private val request_scope = new Task_Scope[Task[?]]

  protected def serve(): Unit = {
    val read = Task.close_on_stop(connection, s"client ${connection.remote}", log)(
      connection.read_line())
    request_scope.run(read).foreach(handle_request)
  }

  def stop(): Unit = {
    request_scope.stop()
    tool_call_scope.stop()
  }
}
