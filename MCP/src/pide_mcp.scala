/*  Title:      MCP/src/pide_mcp.scala
    Author:     Kevin Kappelmann

Entry point for PIDE MCP.
*/

package isabelle.pide.mcp

import isabelle._

import java.net.ServerSocket

object PIDE_MCP {
  def long_running_threshold(options: Options): Time =
    options.seconds("pide_mcp_long_running_threshold")

  private def main(name: String, mcp: Boolean, args: List[String]): Unit = {
    var log_path: Option[Path] = None
    var session_specs: List[PIDE_MCP_Session_Spec] = Nil
    var verbose = false
    var log_messages = false
    var port = if (mcp) None else Some(0)
    var existing_mcp_server = false
    val session_spec = new PIDE_MCP_Session_Spec.Builder

    val getopts = Getopts("""
Usage: isabelle """ + name + """ [OPTIONS]

  Options are:
    -L FILE                log on FILE (next to console stderr)
    -S "SESSION_OPTIONS"   start PIDE session with the given options ("" for defaults)""" +
(if (mcp) """
    -p PORT                also serve command-line and MCP clients on TCP PORT (0 for any free port);
                           if PORT is bound by another server, proxy to it
    -s                     proxy to an existing server on PORT, no own server"""
else """
    -p PORT                TCP port (default: 0, meaning any free port)""") + """
    -v                     verbose
    -w                     log all communication
    plus any session options, starting a PIDE session.
    Passed Isabelle system options are inherited by the sessions given via -S.

  """ + PIDE_MCP_Session_Spec.usage +
(if (mcp) """
  Start an MCP (Model Context Protocol) server that manages headless PIDE sessions over stdin/stdout.
  Proxied startup sessions are started on the server via its cmd protocol.
"""
else """
  Start a TCP server that manages headless PIDE sessions for command-line and MCP clients.
"""),
      List(
        "L:" -> (arg => log_path = Some(PIDE_MCP_Util.path(arg))),
        "S:" -> (arg =>
          session_specs = session_specs ::: List(PIDE_MCP_Session_Spec.parse(arg))),
        "p:" -> (arg => port = Some(Value.Int.parse(arg))),
        "v" -> (_ => verbose = true),
        "w" -> (_ => log_messages = true)) :::
        (if (mcp) List("s" -> ((_: String) => existing_mcp_server = true)) else Nil) :::
        session_spec.option_specs: _*)

    val more_args = getopts(args)
    if (more_args.nonEmpty) getopts.usage()

    val base_options: Options.Update = session_spec.spec.map(_.options).getOrElse(Nil)
    val specs = (if (session_spec.has_session_option) session_spec.spec.toList else Nil) :::
      session_specs.map(spec => spec.copy(options = base_options ::: spec.options))
    val options = Options.init(update = base_options)
    val threshold = long_running_threshold(options)
    val progress = log_path match {
      case None => new Console_Progress(
        verbose = verbose, threshold = threshold, detailed = false, stderr = true)
      case Some(path) => new Console_File_Progress(path,
        verbose = verbose, threshold = threshold, detailed = false, stderr = true)
    }
    val log = Logger.make_progress(progress)
    val message_log = if (log_messages) log else Logger.none
    val address = Server.localhost
    val handshake_timeout = PIDE_AI_Protocol_Handshake.timeout(options)

    def run_server(socket: Option[ServerSocket]): Unit = {
      val protocols = PIDE_AI_Protocol.make_table(
        include_ids = PIDE_AI_Protocol.include_ids(options),
        exclude_ids = PIDE_AI_Protocol.exclude_ids(options))
      val server = new PIDE_AI_Server(options, log, progress,
        PIDE_MCP_Service.make_table(
          include_ids = PIDE_MCP_Service.include_ids(options),
          exclude_ids = PIDE_MCP_Service.exclude_ids(options)),
        PIDE_MCP_Tool.make_table(
          include_ids = PIDE_MCP_Tool.include_ids(options),
          exclude_ids = PIDE_MCP_Tool.exclude_ids(options)),
        PIDE_AI_Client_Info.make_table(
          include_ids = PIDE_AI_Client_Info.include_ids(options),
          exclude_ids = PIDE_AI_Client_Info.exclude_ids(options)),
        specs)
      val stdio =
        if (mcp) Some(new PIDE_AI_Stdio_Connector(server, PIDE_MCP_Protocol, log, 
          message_log, on_client_closed = _ => server.stop()))
        else None
      server.run(stdio.toList ::: socket.toList.map(socket =>
        new PIDE_AI_TCP_Connector(server, socket, protocols, handshake_timeout, log,
          message_log)))
    }

    def run_proxy(port: Int): Unit = {
      val client_connector = new PIDE_AI_TCP_Client_Connector(
        address, port, handshake_timeout, message_log)
      val server = client_connector.remote
      val scope = new Task_Scope[Task[?]]

      def start_session(spec: PIDE_MCP_Session_Spec, progress: Progress): Unit = {
        def fail(message: String): Nothing =
          error(cat_error(s"Cannot start session ${quote(spec.id.getOrElse(spec.logic))} " +
            s"on server $server", message))
        val result =
          try {
            using(client_connector.connect(PIDE_AI_Command_Protocol)) { connection =>
              val call = Task.close_on_stop(connection, s"server $server", log)(Exn.capture {
                PIDE_AI_Command_Client.call_tool(connection, Start_Session_Tool.id, spec.json,
                  progress)
              })
              Exn.release(scope.run(call))
            }
          }
          catch { case ERROR(message) => fail(message) }
        result match {
          case Result.Res(_) =>
          case Result.Error(err) => fail(JSON_Object.text(err))
        }
      }
      val startup = new PIDE_AI_Startup_Sessions(specs,
        await = PIDE_AI_Startup_Sessions.await(options),
        exit_on_failure = PIDE_AI_Startup_Sessions.exit_on_failure(options),
        log, progress, start_session)

      log(s"Proxying ${PIDE_MCP_Protocol.id} to server $server...")
      using(client_connector.connect(PIDE_MCP_Protocol)) { server_connection =>
        // only log TCP (redundancy)
        using(new Stdio_Connection(Logger.none)) { host_connection =>
          val proxy = new PIDE_AI_Proxy(host_connection, server_connection, log)
          try {
            Exn.Interrupt.signal_handler {
              log("Interrupt: shutting down...")
              scope.stop()
            } {
              scope.fork("pide_ai_startup_sessions", startup)
              if (Exn.is_res(startup.ready.join_result)) scope.run(proxy)
              scope.stop()
              scope.join_stop_request()
            }
          }
          finally { scope.join() }
        }
      }
    }

    val result = Exn.capture {
      if (existing_mcp_server) run_proxy(port.getOrElse(error("Option -s requires port")))
      else port match {
        case None => run_server(None)
        case Some(port) => PIDE_AI_TCP_Connector.bind(address, port) match {
          case Result.Error(exn) =>
            if (!mcp) error(s"Cannot bind port $port: ${Exn.message(exn)}")
            else {
              log(s"Port $port not available (${Exn.message(exn)})")
              run_proxy(port)
            }
          case Result.Res(socket) => run_server(Some(socket))
        }
      }
    }
    PIDE_MCP_Util.log_failure(log, name, result)
    result match {
      case Exn.Exn(exn) => sys.exit(Exn.failure_rc(exn))
      case Exn.Res(_) =>
    }
  }

  val isabelle_tool_ai = Isabelle_Tool("pide_ai",
    "start PIDE AI server for command-line and MCP clients", Scala_Project.here,
    args => main("pide_ai", mcp = false, args))
  val isabelle_tool_mcp = Isabelle_Tool("pide_mcp", "start PIDE MCP server", Scala_Project.here,
    args => main("pide_mcp", mcp = true, args))
}

class PIDE_MCP_Application
  extends Isabelle_Scala_Tools(PIDE_MCP.isabelle_tool_ai, PIDE_MCP.isabelle_tool_mcp)
