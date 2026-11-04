/*  Title:      MCP/src/pide_ai_stdio_connector.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.mcp

import isabelle._

class PIDE_AI_Stdio_Connector(
  server: PIDE_AI_Server,
  protocol: PIDE_AI_Protocol,
  log: Logger,
  message_log: Logger,
  on_client_closed: PIDE_AI_Client_Handler[Stdio_Connection] => Unit = _ => ()
) extends PIDE_AI_Connector[Stdio_Connection]("pide_ai_stdio", server, log,
    Map(protocol.id -> protocol), on_client_closed) {
  private val pending = Synchronized(true)

  def start_message: String = s"Serving ${protocol.id} on stdin/stdout..."

  def accept_connection(): Option[Stdio_Connection] =
    Option.when(pending.change_result(pending => (pending, false)))(
      new Stdio_Connection(message_log))

  def connection_protocol(connection: Stdio_Connection): Option[PIDE_AI_Protocol] =
    Some(protocol)

  def accepting_connections: Boolean = pending.value
  def stop_accepting_connections(): Unit = pending.change(_ => false)
}
