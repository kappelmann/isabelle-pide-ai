/*  Title:      AI/src/pide_ai.scala
    Author:     Kevin Kappelmann

Entry point for PIDE AI.
*/

package isabelle.pide.ai

import isabelle._

import java.net.ServerSocket

object PIDE_AI {
  def log_long_running_threshold(options: Options): Time =
    options.seconds("pide_ai_log_long_running_threshold")

  class Context(
    val options: Options,
    val specs: List[PIDE_AI_Session_Spec],
    val port: Option[Int],
    val progress: Progress,
    val log: Logger,
    val message_log: Logger
  ) {
    val address = Server.localhost
    val handshake_timeout = PIDE_AI_Protocol_Handshake.timeout(options)

    def tcp_connector(server: PIDE_AI_Server, socket: ServerSocket): PIDE_AI_TCP_Connector = {
      val protocols = PIDE_AI_Protocol.make_table(
        include_ids = PIDE_AI_Protocol.include_ids(options),
        exclude_ids = PIDE_AI_Protocol.exclude_ids(options))
      new PIDE_AI_TCP_Connector(server, socket, protocols, handshake_timeout, log, message_log)
    }

    def run_server(connectors: PIDE_AI_Server => List[PIDE_AI_Connector[?]]): Unit = {
      val server = new PIDE_AI_Server(options, log, progress,
        PIDE_AI_Service.make_table(
          include_ids = PIDE_AI_Service.include_ids(options),
          exclude_ids = PIDE_AI_Service.exclude_ids(options)),
        PIDE_AI_Tool.make_table(
          include_ids = PIDE_AI_Tool.include_ids(options),
          exclude_ids = PIDE_AI_Tool.exclude_ids(options)),
        PIDE_AI_Client_Info.make_table(
          include_ids = PIDE_AI_Client_Info.include_ids(options),
          exclude_ids = PIDE_AI_Client_Info.exclude_ids(options)),
        specs)
      server.run(connectors(server))
    }

    def run_proxy(port: Int, protocol: PIDE_AI_Protocol, host_connection: Line_Connection): Unit = {
      val client_connector = new PIDE_AI_TCP_Client_Connector(
        address, port, handshake_timeout, message_log)
      val server = client_connector.remote
      val scope = new Task_Scope[Task[?]]

      def start_session(spec: PIDE_AI_Session_Spec, progress: Progress): Unit = {
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
          catch { case ERROR(message) => PIDE_AI_Tool_Result.Error(message) }
        result match {
          case Result.Res(_) =>
          case Result.Error(err) =>
            error(cat_error(s"Cannot start session ${quote(spec.id.getOrElse(spec.logic))} " +
              s"on server $server", JSON_Object.text(err)))
        }
      }
      val startup = new PIDE_AI_Startup_Sessions(specs,
        await = PIDE_AI_Startup_Sessions.await(options),
        exit_on_failure = PIDE_AI_Startup_Sessions.exit_on_failure(options),
        log, progress, start_session)

      log(s"Proxying ${protocol.id} to server $server...")
      using(client_connector.connect(protocol)) { server_connection =>
        val proxy = new Line_Connection_Proxy(host_connection, server_connection, log)
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

  def main(
    name: String,
    args: List[String],
    port_usage: String,
    description: String,
    more_option_specs: List[(String, String => Unit)] = Nil
  )(run: Context => Unit): Unit = {
    var log_path: Option[Path] = None
    var session_specs: List[PIDE_AI_Session_Spec] = Nil
    var verbose = false
    var log_messages = false
    var port: Option[Int] = None
    val session_spec = new PIDE_AI_Session_Spec.Builder

    val getopts = Getopts("""
Usage: isabelle """ + name + """ [OPTIONS]

  Options are:
    -L FILE                log on FILE (next to console stderr)
    -S "SESSION_OPTIONS"   start PIDE session with the given options ("" for defaults)""" +
      port_usage + """
    -v                     verbose
    -w                     log all communication
    plus any session options, starting a PIDE session.
    Passed Isabelle system options are inherited by the sessions given via -S.

  """ + PIDE_AI_Session_Spec.usage + description,
      List(
        "L:" -> (arg => log_path = Some(PIDE_AI_Util.path(arg))),
        "S:" -> (arg =>
          session_specs = session_specs ::: List(PIDE_AI_Session_Spec.parse(arg))),
        "p:" -> (arg => port = Some(Value.Int.parse(arg))),
        "v" -> (_ => verbose = true),
        "w" -> (_ => log_messages = true)) :::
        more_option_specs ::: session_spec.option_specs: _*)

    val more_args = getopts(args)
    if (more_args.nonEmpty) getopts.usage()

    val base_options: Options.Update = session_spec.spec.map(_.options).getOrElse(Nil)
    val specs = (if (session_spec.has_session_option) session_spec.spec.toList else Nil) :::
      session_specs.map(spec => spec.copy(options = base_options ::: spec.options))
    val options = Options.init(update = base_options)
    val threshold = log_long_running_threshold(options)
    val console = new Console_Progress(verbose = verbose, threshold = threshold,
      detailed = false, stderr = true)
    val files = log_path.toList.map(path =>
      new File_Progress(path, verbose = verbose) {
        override def status_threshold: Time = threshold
      })
    val progress = new Tee_Progress((console :: files)*) with Progress.Global_Interrupts
    val log = Logger.make_progress(progress)
    val message_log = if (log_messages) log else Logger.none

    val result = Exn.capture {
      run(new Context(options, specs, port, progress, log, message_log))
    }
    PIDE_AI_Util.log_failure(log, name, result)
    result match {
      case Exn.Exn(exn) => sys.exit(Exn.failure_rc(exn))
      case Exn.Res(_) =>
    }
  }

  val isabelle_tool = Isabelle_Tool("pide_ai", "start PIDE AI server", Scala_Project.here,
    args => main("pide_ai", args,
      port_usage = """
    -p PORT                TCP port (default: 0, meaning any free port)""",
      description = """
  Start a PIDE AI server on a TCP port, serving all enabled protocols.
""") { context =>
      val port = context.port.getOrElse(0)
      PIDE_AI_TCP_Connector.bind(context.address, port) match {
        case Result.Error(exn) => error(s"Cannot bind port $port: ${Exn.message(exn)}")
        case Result.Res(socket) =>
          context.run_server(server => List(context.tcp_connector(server, socket)))
      }
    })
}

class PIDE_AI_Application extends Isabelle_Scala_Tools(PIDE_AI.isabelle_tool)
