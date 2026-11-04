/*  Title:      MCP/src/pide_ai_connector.scala
    Author:     Kevin Kappelmann

Creating client handlers.
*/

package isabelle.pide.mcp

import isabelle._

abstract class PIDE_AI_Connector[C <: Line_Connection](
  val id: String,
  val server: PIDE_AI_Server,
  val log: Logger,
  val protocols: Map[String, PIDE_AI_Protocol],
  on_client_closed: PIDE_AI_Client_Handler[C] => Unit
) extends Task[Unit] {
  def start_message: String
  def accept_connection(): Option[C]
  def accepting_connections: Boolean
  def stop_accepting_connections(): Unit
  def connection_protocol(connection: C): Option[PIDE_AI_Protocol]

  val client_handler_scope = new Task_Scope[PIDE_AI_Client_Handler[C]]
  def client_handlers: List[PIDE_AI_Client_Handler[C]] = client_handler_scope.tasks

  final def run(): Unit = {
    log(start_message)
    try {
      val connections = Iterator.continually(accept_connection()).takeWhile(_.isDefined).flatten
      for (connection <- connections) {
        val handler = new PIDE_AI_Client_Handler(this, connection, log)
        client_handler_scope.fork("pide_ai_client_handler", handler,
          on_finish = _ => on_client_closed(handler))
      }
    }
    finally { client_handler_scope.join() }
  }

  final def stop(): Unit = {
    stop_accepting_connections()
    client_handler_scope.stop()
  }
}
