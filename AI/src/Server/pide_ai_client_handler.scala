/*  Title:      AI/src/Server/pide_ai_client_handler.scala
    Author:     Kevin Kappelmann

Selects a connection's protocol and runs the protocol's endpoint.
*/

package isabelle.pide.ai

import isabelle._

class PIDE_AI_Client_Handler[C <: Line_Connection](
  val connector: PIDE_AI_Connector[C],
  val connection: C,
  log: Logger
) extends Task[Unit] {
  private val scope = new Task_Scope[Task[?]]
  private val handshake =
    Task.close_on_stop(connection, s"client ${connection.remote}", log)(
      connector.connection_protocol(connection))

  def run(): Unit =
    try {
      using(connection)(_ =>
        for (protocol <- scope.run(handshake)) {
          log(s"Client ${connection.remote} connected (${protocol.id})")
          try { scope.run(protocol.endpoint(connector.server, connection, log)) }
          finally {
            Exn.capture { log(s"Disconnected from client ${connection.remote} (${protocol.id})") }
          }
        })
    }
    catch {
      case exn: Throwable =>
        PIDE_AI_Util.log_failure(log, s"Client ${connection.remote} failure", Exn.Exn(exn))
    }

  def stop(): Unit = scope.stop()
}
