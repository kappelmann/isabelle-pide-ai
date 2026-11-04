/*  Title:      MCP/src/line_connection.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.mcp

import isabelle._

import java.io.{BufferedInputStream, BufferedOutputStream, InputStream, IOException, OutputStream}
import java.net.{InetSocketAddress, Socket, SocketAddress, SocketTimeoutException}

abstract class Line_Connection(
  in: InputStream,
  out: OutputStream,
  log: Logger
) extends AutoCloseable {
  def remote: String
  def shutdown_output(): Unit

  private val input = new BufferedInputStream(in)
  private val output = new BufferedOutputStream(out)
  val closed = Synchronized(false)

  def read_line(): Option[String] = {
    val line = try { Byte_Message.read_line(input).map(_.text) }
      catch { case _: IOException if closed.value => None }
    for (text <- line) log(s"IN $remote: $text")
    line
  }

  def read_lines(): Iterator[String] =
    Iterator.continually(read_line()).takeWhile(_.isDefined).flatten
  def write_line(line: String): Unit = {
    log(s"OUT $remote: $line")
    output.synchronized { Byte_Message.write_line(output, Bytes(line)) }
  }

  def forward(to: Line_Connection): Unit = read_lines().foreach(to.write_line)

  def close(): Unit = closed.change(_ => true)
}

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

class Stdio_Connection(log: Logger) extends Line_Connection(System.in.nn, System.out.nn, log) {
  val remote = "stdin/stdout"
  def shutdown_output(): Unit = ()
}
