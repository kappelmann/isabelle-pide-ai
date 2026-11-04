/*  Title:      AI/src/Connection/line_connection.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.ai

import isabelle._

import java.io.{BufferedInputStream, BufferedOutputStream, InputStream, IOException, OutputStream}

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
