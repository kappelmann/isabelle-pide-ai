/*  Title:      AI/src/TCP/pide_ai_tcp_connector.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.ai

import isabelle._

import java.net.{BindException, InetAddress, ServerSocket}

object PIDE_AI_TCP_Connector {
  def bind(
    address: InetAddress,
    port: Int,
    backlog: Int = 50
  ): Result[ServerSocket, BindException] =
    try { Result.Res(new ServerSocket(port, backlog, address)) }
    catch { case exn: BindException => Result.Error(exn) }
}

class PIDE_AI_TCP_Connector(
  server: PIDE_AI_Server,
  socket: ServerSocket,
  protocols: Map[String, PIDE_AI_Protocol],
  handshake_timeout: Time,
  log: Logger,
  message_log: Logger,
  on_client_closed: PIDE_AI_Client_Handler[Socket_Connection] => Unit = _ => ()
) extends PIDE_AI_Connector[Socket_Connection]("pide_ai_tcp", server, log, protocols,
    on_client_closed) {
  def address: InetAddress = socket.getInetAddress.nn
  def port: Int = socket.getLocalPort

  def connections: List[Socket_Connection] = client_handlers.map(_.connection)

  def start_message: String = s"Serving TCP clients on port $port..."

  def accepting_connections: Boolean = !socket.isClosed
  def stop_accepting_connections(): Unit =
    PIDE_AI_Util.log_failure(log, s"Failed to close TCP connector on port $port",
      Exn.capture { socket.close() })

  def accept_connection(): Option[Socket_Connection] =
    Exn.capture(socket.accept.nn) match {
      case Exn.Res(client) => Some(new Socket_Connection(client, message_log))
      case Exn.Exn(exn) =>
        if (accepting_connections) {
          PIDE_AI_Util.log_failure(log, s"TCP connector failure on port $port", Exn.Exn(exn))
          stop_accepting_connections()
        }
        None
    }

  def connection_protocol(connection: Socket_Connection): Option[PIDE_AI_Protocol] =
    connection.with_timeout(handshake_timeout,
      error(s"No protocol name within ${handshake_timeout.message}")) {
      PIDE_AI_Protocol_Handshake.read_protocol(protocols, connection)
    }
}
