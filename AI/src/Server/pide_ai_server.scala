/*  Title:      AI/src/Server/pide_ai_server.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.ai

import isabelle._

class PIDE_AI_Server(
  options: Options,
  val log: Logger,
  val base_progress: Progress.Status,
  val service_table: Map[String, PIDE_AI_Service],
  val tool_table: Map[String, PIDE_AI_Tool],
  val client_info_table: Map[String, PIDE_AI_Client_Info],
  specs: List[PIDE_AI_Session_Spec]
) {
  def services: List[PIDE_AI_Service] = service_table.values.toList.sortBy(_.id)
  def service(id: String): Option[PIDE_AI_Service] = service_table.get(id)
  def tools: List[PIDE_AI_Tool] = tool_table.values.toList.sortBy(_.id)
  def tool(id: String): Option[PIDE_AI_Tool] = tool_table.get(id)

  private val connectors_state = Synchronized[List[PIDE_AI_Connector[?]]](Nil)
  def connectors: List[PIDE_AI_Connector[?]] = connectors_state.value
  def client_infos: List[PIDE_AI_Client_Info] = client_info_table.values.toList.sortBy(_.id)

  def access_note: Option[String] =
    proper_string((for {
      connector <- connectors if connector.accepting_connections
      info <- client_infos if connector.protocols.contains(info.protocol.id)
      note <- info.access_note(connector)
    } yield note).mkString(" "))

  val scope = new Task_Scope[Task[?]]
  def stop(): Unit = scope.stop()

  def run_services(operation: String, session: PIDE_AI_Session)(
    run: PIDE_AI_Service => Unit
  ): Unit = {
    val services = service_table.values.toList ::: tool_table.values.toList
    PIDE_AI_Util.release_combined(services.map(service =>
      Exn.capture { run(service) } match {
        case Exn.Exn(exn) if !Exn.is_interrupt(exn) =>
          Exn.Exn(ERROR(s"Fatal error during $operation of ${service.kind} " +
            s"${quote(service.id)} for session ${quote(session.id)}: ${Exn.message(exn)}"))
        case result => result
      }))
  }

  val sessions = new PIDE_AI_Sessions(new PIDE_AI_Sessions.Session_Hooks {
      def service_event(event: PIDE_AI_Sessions.Session_Event): PIDE_AI_Service.Session_Event =
        PIDE_AI_Service.Session_Event(PIDE_AI_Server.this, event.session, event.progress)
      override def started(event: PIDE_AI_Sessions.Session_Event): Unit =
        run_services("start", event.session) { service =>
          event.progress.expose_interrupt()
          service.started(service_event(event))
        }
      override def stopping(event: PIDE_AI_Sessions.Session_Event): Unit =
        run_services("stop", event.session)(_.stopping(service_event(event)))
    }, log, base_progress, options)

  val startup = new PIDE_AI_Startup_Sessions(specs,
    await = PIDE_AI_Startup_Sessions.await(options),
    exit_on_failure = PIDE_AI_Startup_Sessions.exit_on_failure(options),
    log, base_progress, (spec, progress) => sessions.start_session(spec, progress))

  def ready: Future[Unit] = startup.ready

  def shutdown(): Unit =
    PIDE_AI_Util.release_combined(List(
      Exn.capture { scope.stop() },
      Exn.capture { scope.join() },
      Exn.capture { log("Stopping PIDE sessions...") },
      Exn.capture { sessions.stop_sessions(None, base_progress) }))

  def run(connectors: List[PIDE_AI_Connector[?]]): Unit = {
    val result = Exn.capture {
      Exn.Interrupt.signal_handler {
        log("Interrupt: shutting down...");
        stop()
      } {
        scope.fork("pide_ai_startup_sessions", startup)
        connectors_state.change(_ => connectors)
        for (connector <- connectors) scope.fork(connector.id, connector)
        scope.join_stop_request()
      }
    }
    val shutdown_result = Exn.capture { shutdown() }
    PIDE_AI_Util.release_combined(List(result, shutdown_result))
  }
}
