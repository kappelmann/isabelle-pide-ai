/*  Title:      AI/src/Connection/line_connection_proxy.scala
    Author:     Kevin Kappelmann

Bridges a host connection to a server connection.
*/

package isabelle.pide.ai

import isabelle._

object Line_Connection_Proxy {
  def forward(from: Line_Connection, to: Line_Connection, log: Logger): Task[Unit] =
    Task.close_on_stop(from, from.remote, log) {
      from.forward(to)
      to.shutdown_output()
    }
}

class Line_Connection_Proxy(
  host_connection: Line_Connection,
  server_connection: Line_Connection,
  log: Logger
) extends Task[Unit] {
  private val scope = new Task_Scope[Task[?]]

  def run(): Unit =
    try {
      scope.fork("pide_ai_proxy_to_server",
        Line_Connection_Proxy.forward(host_connection, server_connection, log), daemon = true)
      scope.run(Line_Connection_Proxy.forward(server_connection, host_connection, log))
      scope.stop()
      scope.join_stop_request()
    }
    finally { Exn.capture { log(s"Disconnected from server ${server_connection.remote}") } }

  def stop(): Unit = scope.stop()
}
