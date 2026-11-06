/*  Title:      MCP/src/pide_mcp.scala
    Author:     Kevin Kappelmann

Entry point for PIDE MCP.
*/

package isabelle.pide.mcp

import isabelle._
import isabelle.pide.ai._

import java.net.ServerSocket

object PIDE_MCP {
  val isabelle_tool = Isabelle_Tool("pide_mcp", "start PIDE MCP server", Scala_Project.here,
    { args =>
      var existing_server = false
      PIDE_AI.main("pide_mcp", args,
        port_usage = """
    -p PORT                also serve all enabled protocols on TCP PORT (0 for any free port);
                           if PORT is bound by another server, proxy to it
    -s                     proxy to an existing server on PORT, no own server""",
        description = """
  Start a PIDE MCP (Model Context Protocol) server over stdin/stdout.
  Proxied startup sessions are started via the command protocol.
""",
        more_option_specs = List("s" -> (_ => existing_server = true))) { context =>
        def run_server(socket: Option[ServerSocket]): Unit =
          context.run_server(server =>
            new PIDE_AI_Stdio_Connector(server, PIDE_MCP_Protocol, context.client_context,
              context.log, context.message_log, on_client_closed = _ => server.stop()) ::
            socket.toList.map(context.tcp_connector(server, _)))

        def run_proxy(port: Int): Unit =
          // only log TCP (redundancy)
          using(new Stdio_Connection(Logger.none))(
            context.run_proxy(port, PIDE_MCP_Protocol, _))

        if (existing_server) run_proxy(context.port.getOrElse(error("Option -s requires port")))
        else context.port match {
          case None => run_server(None)
          case Some(port) => PIDE_AI_TCP_Connector.bind(context.address, port) match {
            case Result.Error(exn) =>
              context.log(s"Port $port not available (${Exn.message(exn)})")
              run_proxy(port)
            case Result.Res(socket) => run_server(Some(socket))
          }
        }
      }
    })
}

class PIDE_MCP_Application extends Isabelle_Scala_Tools(PIDE_MCP.isabelle_tool)
