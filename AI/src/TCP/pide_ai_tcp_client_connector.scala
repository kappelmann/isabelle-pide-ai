/*  Title:      AI/src/TCP/pide_ai_tcp_client_connector.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.ai

import isabelle._

import java.net.{ConnectException, InetAddress, InetSocketAddress, Socket}

class PIDE_AI_TCP_Client_Connector(
  val address: InetAddress,
  val port: Int,
  handshake_timeout: Time,
  message_log: Logger
) extends PIDE_AI_Client_Connector {
  def remote: String =
    Socket_Connection.address_name(new InetSocketAddress(address, port))

  def connect(protocol: PIDE_AI_Protocol): Socket_Connection = {
    val socket = try { new Socket(address, port) }
      catch {
        case exn: ConnectException =>
          error(s"Cannot connect to server $remote: ${Exn.message(exn)}")
      }
    val connection = new Socket_Connection(socket, message_log)
    try {
      connection.with_timeout(handshake_timeout,
        error(s"No answer to protocol handshake for ${quote(protocol.id)} " +
          s"on $remote within ${handshake_timeout.message}")) {
        PIDE_AI_Protocol_Handshake.request_protocol(protocol, connection, remote)
      }
      connection
    }
    catch {
      case exn: Throwable =>
        Exn.capture { connection.close() }
        throw exn
    }
  }
}
