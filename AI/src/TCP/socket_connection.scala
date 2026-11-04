/*  Title:      AI/src/TCP/socket_connection.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.ai

import isabelle._

import java.io.IOException
import java.net.{InetSocketAddress, Socket, SocketAddress, SocketTimeoutException}

object Socket_Connection {
  def address_name(address: SocketAddress | Null): String =
    address match {
      case address: InetSocketAddress =>
        val host = address.getHostString.nn
        (if (host.contains(':')) "[" + host + "]" else host) + ":" + address.getPort
      case address => String.valueOf(address).nn
    }
}

class Socket_Connection(socket: Socket, log: Logger)
extends Line_Connection(socket.getInputStream.nn, socket.getOutputStream.nn, log) {
  val remote: String = Socket_Connection.address_name(socket.getRemoteSocketAddress)
  def shutdown_output(): Unit = socket.shutdownOutput()

  def with_timeout[A](timeout: Time, on_timeout: => A)(body: => A): A = {
    socket.setSoTimeout(timeout.ms.toInt)
    try { body }
    catch { case _: SocketTimeoutException => on_timeout }
    finally {
      try { socket.setSoTimeout(0) }
      catch { case _: IOException if closed.value => }
    }
  }

  override def close(): Unit = {
    super.close()
    socket.close()
  }
}
