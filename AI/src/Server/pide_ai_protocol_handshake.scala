/*  Title:      AI/src/Server/pide_ai_protocol_handshake.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.ai

import isabelle._

object PIDE_AI_Protocol_Handshake {
  def timeout(options: Options): Time = options.seconds("pide_ai_protocol_handshake_timeout")

  val OK = "OK"
  val error_response = JSON_Schema.Format.property("error", JSON_Schema.Format.string)

  def error_message(reply: String): String =
    reply match {
      case JSON.Format(error_response(message)) => message
      case _ => reply
    }

  def read_protocol(
    protocols: Map[String, PIDE_AI_Protocol],
    connection: Line_Connection
  ): Option[PIDE_AI_Protocol] =
    connection.read_line().flatMap { name =>
      val protocol = protocols.get(name)
      connection.write_line(if (protocol.isDefined) OK
        else JSON.Format(error_response(s"Bad protocol: ${quote(name)}")))
      protocol
    }

  def request_protocol(
    protocol: PIDE_AI_Protocol,
    connection: Line_Connection,
    server: String
  ): Unit = {
    connection.write_line(protocol.id)
    connection.read_line() match {
      case Some(OK) =>
      case Some(line) => error(s"Server $server: ${error_message(line)}")
      case None => error(s"Server $server closed the connection")
    }
  }
}
