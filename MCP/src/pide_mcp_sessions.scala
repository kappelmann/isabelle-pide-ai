/*  Title:      MCP/src/pide_mcp_sessions.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.mcp

import isabelle._

import scala.collection.immutable.VectorMap

object PIDE_MCP_Sessions {
  def build_delay(options: Options): Time = options.seconds("build_delay")

  sealed case class Session_Event(session: PIDE_MCP_Session, progress: Progress)
  type Session_Hooks = Lifecycle_Hooks[Session_Event, Session_Event]
}

class PIDE_MCP_Sessions(
  hooks: PIDE_MCP_Sessions.Session_Hooks,
  val log: Logger,
  val base_progress: Progress,
  val options: Options
) {
  private val build_lock = new Queue_Lock

  private enum Phase { case Building, Starting, Running, Stopping }

  private val state = Synchronized[VectorMap[String, Entry]](VectorMap.empty)

  private def state_json(entries: VectorMap[String, Entry]): JSON.Object.T = {
    val grouped = entries.toList.groupBy { case (_, entry) => entry.phase }
    JSON_Object(
      for (phase <- Phase.values.toList; ids <- grouped.get(phase))
        yield Word.lowercase(phase.toString) -> ids.map(_._1))
  }
  def state_json(): JSON.Object.T = state_json(state.value)

  private def remove_session(session_id: String): Unit = state.change(_ - session_id)

  private def select[A](
    entries: VectorMap[String, Entry],
    what: String,
    candidates: List[(String, A)],
    session_ids: Option[List[String]]
  ): List[(String, A)] =
    session_ids match {
      case None => candidates
      case Some(ids) =>
        val table = candidates.toMap
        val (missing, found) = ids.partitionMap(id => table.get(id).map(id -> _).toRight(id))
        if (missing.nonEmpty) error(
          s"No $what session(s) ${commas_quote(missing)} found. " +
          s"Available session(s): ${JSON.Format(state_json(entries))}")
        else found
    }

  def running_sessions(session_ids: Option[List[String]]): List[PIDE_MCP_Session] = {
    val entries = state.value
    val running = entries.toList.flatMap { case (id, entry) => entry.running.map(id -> _) }
    select(entries, "running", running, session_ids).map(_._2)
  }

  private def fresh_id(base: String, entries: VectorMap[String, Entry]): String =
    if (!entries.contains(base)) base
    else Iterator.from(2).map(base + "#" + _).find(!entries.contains(_)).get

  private class Entry(val id: String, spec: PIDE_MCP_Session_Spec, caller_progress: Progress) {
    private val session_future = Future.promise[PIDE_MCP_Session]
    private val start_thread = Synchronized[Option[Thread]](None)
    private val start_progress = new Sub_Progress(caller_progress)
    private val built = Synchronized(false)
    private val phase_consumer = Synchronized[Option[Session.Consumer[Session.Phase]]](None)
    private val stop_requested = Synchronized(false)
    private val stopped = Future.promise[Unit]

    private def request_stop(): Boolean =
      stop_requested.change_result(requested => (!requested, true))

    def phase: Phase = if (stop_requested.value) Phase.Stopping
      else session_future.peek match {
        case None => if (built.value) Phase.Starting else Phase.Building
        case Some(Exn.Res(_)) => Phase.Running
        case Some(Exn.Exn(_)) => Phase.Stopping // a failed start automatically stops
      }

    def running: Option[PIDE_MCP_Session] =
      if (phase == Phase.Running) session_future.peek.collect { case Exn.Res(session) => session }
      else None

    private def shutdown(session: PIDE_MCP_Session, progress: Progress): Unit = {
      Exn.capture { log(s"Stopping PIDE session ${quote(id)}...") }
      for (consumer <- phase_consumer.change_result(consumer => (consumer, None)))
        session.session.phase_changed -= consumer
      PIDE_MCP_Util.release_combined(List(
        Exn.capture {
          hooks.stopping(
            PIDE_MCP_Sessions.Session_Event(session, new Unstoppable_Progress(progress)))
        },
        Exn.capture { Isabelle_Thread.perhaps_uninterruptible { session.stop() } } match {
          case Exn.Res(process_result) =>
            if (!process_result.ok) Exn.capture {
              log.error_message(s"PIDE session ${quote(id)} stopped with " +
                PIDE_MCP_Util.print_process_result(process_result))
            }
            Exn.Res(())
          case Exn.Exn(exn) if !Exn.is_interrupt(exn) =>
            Exn.Exn(ERROR(s"Error stopping PIDE session ${quote(id)}: ${Exn.message(exn)}"))
          case Exn.Exn(exn) => Exn.Exn(exn)
        }))
    }

    def start(): PIDE_MCP_Session = {
      val started_before = start_thread.change_result {
        case None => (false, Some(Thread.currentThread().nn))
        case starter => (true, starter)
      }
      if (started_before) error(s"PIDE session ${quote(id)} was already started")
      var started: Option[PIDE_MCP_Session] = None
      try {
        log(s"Starting PIDE session ${quote(id)} " +
          s"with base session ${quote(spec.logic)}...")
        start_progress.expose_interrupt()
        val (session_options, session_background) = build_lock.with_lock(start_progress,
          s"Awaiting build queue for session ${quote(id)}", PIDE_MCP_Sessions.build_delay(options),
          PIDE_MCP_Session.progress_delay(options)) {
          PIDE_MCP_Session.build(spec, start_progress)
        }
        built.change(_ => true)
        start_progress.expose_interrupt()
        val session =
          PIDE_MCP_Session(spec, session_options, session_background, log, start_progress)
        started = Some(session)
        val consumer = Session.Consumer[Session.Phase](this.class_name) {
          case Session.Terminated(result) if !stop_requested.value =>
            Exn.capture {
              log.error_message(s"PIDE session ${quote(id)} terminated unexpectedly: " +
                PIDE_MCP_Util.print_process_result(result))
            }
            Isabelle_Thread.fork(name = "pide_mcp_stop_" + id) {
              PIDE_MCP_Util.log_failure(log, s"Stopping terminated PIDE session ${quote(id)}",
                Exn.capture { stop(base_progress) })
            }
          case _ =>
        }
        phase_consumer.change(_ => Some(consumer))
        session.session.phase_changed += consumer
        consumer.consume(session.session.phase) // session might have stopped already
        start_progress.expose_interrupt()
        log(s"PIDE session ${quote(id)} started")
        hooks.started(PIDE_MCP_Sessions.Session_Event(session, start_progress))
        if (stop_requested.value || !session.session.is_ready)
          error(s"PIDE session ${quote(id)} was stopped during startup")
        session_future.fulfill(session)
        session
      }
      catch {
        case exn: Throwable =>
          val is_owner = request_stop()
          val shutdown_result =
            try { Exn.capture { started.foreach(shutdown(_, start_progress)) } }
            finally { remove_session(id) }
          if (is_owner) stopped.fulfill_result(shutdown_result)
          val start_failure =
            if (!is_owner && Exn.is_interrupt(exn))
              ERROR(s"PIDE session ${quote(id)} was stopped during startup")
            else exn
          val failure = PIDE_MCP_Util.combined_failure(start_failure,
            List(shutdown_result).collect(Exn.the_exn))
          session_future.fulfill_result(Exn.Exn(failure))
          throw failure
      }
    }

    def stop(progress: Progress): Unit = {
      if (request_stop()) stopped.fulfill_result(Exn.capture {
        start_progress.stop()
        if (!start_thread.value.contains(Thread.currentThread().nn) || session_future.is_finished)
          session_future.join_result match {
            case Exn.Exn(_) =>
            case Exn.Res(session) =>
              try { shutdown(session, progress) }
              finally { remove_session(id) }
          }
      })
      stopped.join
    }
  }

  def start_session(
    spec: PIDE_MCP_Session_Spec,
    progress: Progress
  ): PIDE_MCP_Session = {
    val entry = state.change_result { entries =>
      val session_id = spec.id.getOrElse(fresh_id(spec.logic, entries))
      if (session_id.isEmpty) error("Session id must not be empty")
      else if (entries.contains(session_id))
        error(s"Session id ${quote(session_id)} is already used")
      else {
        val entry = new Entry(session_id, spec.copy(id = Some(session_id)), progress)
        (entry, entries + (session_id -> entry))
      }
    }
    entry.start()
  }

  def stop_sessions(session_ids: Option[List[String]], progress: Progress): List[String] = {
    val entries = state.value
    val selected = session_ids match {
      case None => entries.toList
      case Some(ids) =>
        val stoppable = entries.toList.filter { case (_, entry) => entry.phase != Phase.Stopping }
        select(entries, "stoppable", stoppable, Some(ids.distinct))
    }
    PIDE_MCP_Util.release_combined(
      selected.map { case (_, entry) => Exn.capture { entry.stop(progress) } })
    selected.map(_._1)
  }
}
