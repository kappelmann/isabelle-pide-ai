/*  Title:      AI/src/PIDE/pide_ai_sessions.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.ai

import isabelle._

import scala.collection.immutable.VectorMap

object PIDE_AI_Sessions {
  def build_delay(options: Options): Time = options.seconds("build_delay")

  sealed case class Session_Event(session: PIDE_AI_Session, progress: PIDE_AI_Progress)
  type Session_Hooks = Lifecycle_Hooks[Session_Event, Session_Event]
}

class PIDE_AI_Sessions(
  hooks: PIDE_AI_Sessions.Session_Hooks,
  val log: Logger,
  val base_progress: PIDE_AI_Progress,
  val options: Options
) {
  private val build_lock = new Queue_Lock

  private enum Phase { case Building, Starting, Running, Stopping }

  private type Entries = VectorMap[String, (Entry, Task.Forked[Unit])]
  private val state = Synchronized[Entries](VectorMap.empty)

  private def state_json(entries: Entries): JSON.Object.T = {
    val grouped = entries.toList.groupBy { case (_, (entry, _)) => entry.phase }
    JSON_Object(
      for (phase <- Phase.values.toList; ids <- grouped.get(phase))
        yield Word.lowercase(phase.toString) -> ids.map(_._1))
  }
  def state_json(): JSON.Object.T = state_json(state.value)

  private def select[A](
    entries: Entries,
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

  def running_sessions(session_ids: Option[List[String]]): List[PIDE_AI_Session] = {
    val entries = state.value
    val running = entries.toList.flatMap { case (id, (entry, _)) => entry.running.map(id -> _) }
    select(entries, "running", running, session_ids).map(_._2)
  }

  private def fresh_id(base: String, entries: Entries): String =
    if (!entries.contains(base)) base
    else Iterator.from(2).map(base + "#" + _).find(!entries.contains(_)).get

  private class Entry(
    val id: String,
    base_options: Options,
    spec: PIDE_AI_Session_Spec,
    caller_progress: PIDE_AI_Progress
  ) extends Task[Unit] {
    val started = Future.promise[PIDE_AI_Session]
    private val start_progress = new Sub_Progress(caller_progress)
    private val built = Synchronized(false)
    private val stop_request = Synchronized[Option[PIDE_AI_Progress]](None)
    private val terminated = Synchronized(false)

    def stopping: Boolean = stop_request.value.isDefined

    def request_stop(progress: PIDE_AI_Progress): Unit = {
      stop_request.change(_.orElse(Some(progress)))
      start_progress.stop()
    }
    def stop(): Unit = request_stop(base_progress)

    def phase: Phase = if (stopping) Phase.Stopping
      else if (started.is_finished) Phase.Running
      else if (built.value) Phase.Starting
      else Phase.Building

    def running: Option[PIDE_AI_Session] =
      if (stopping) None else started.peek.collect { case Exn.Res(session) => session }

    private def shutdown(
      session: Option[PIDE_AI_Session],
      progress: PIDE_AI_Progress
    ): Exn.Result[Unit] = {
      val result = Exn.capture {
        for (session <- session) {
          Exn.capture { log(s"Stopping PIDE session ${quote(id)}...") }
          PIDE_AI_Util.release_combined(List(
            Exn.capture {
              hooks.stopping(
                PIDE_AI_Sessions.Session_Event(session, new Unstoppable_Progress(progress)))
            },
            Exn.capture { Isabelle_Thread.perhaps_uninterruptible { session.stop() } } match {
              case Exn.Res(process_result) =>
                if (!process_result.ok) Exn.capture {
                  log.error_message(s"PIDE session ${quote(id)} stopped with " +
                    PIDE_AI_Util.print_process_result(process_result))
                }
                Exn.Res(())
              case Exn.Exn(exn) if !Exn.is_interrupt(exn) =>
                Exn.Exn(ERROR(s"Error stopping PIDE session ${quote(id)}: ${Exn.message(exn)}"))
              case Exn.Exn(exn) => Exn.Exn(exn)
            }))
        }
      }
      state.change(_ - id)
      result
    }

    def run(): Unit = {
      def stopped_during_startup: Throwable =
        ERROR(s"Session ${quote(id)} was stopped during startup")
      var created: Option[PIDE_AI_Session] = None
      val start_result = Exn.capture {
        log(s"Starting PIDE session ${quote(id)} " +
          s"with base session ${quote(spec.logic)}...")
        start_progress.expose_interrupt()
        val (session_options, session_background) = build_lock.with_lock(start_progress,
          elapsed => s"Awaiting build queue for session ${quote(id)} " +
            PIDE_AI_Progress.elapsed_text(elapsed),
          PIDE_AI_Sessions.build_delay(options),
          PIDE_AI_Session.progress_delay(options)) {
          PIDE_AI_Session.build(base_options, spec, start_progress)
        }
        built.change(_ => true)
        start_progress.expose_interrupt()
        val session =
          PIDE_AI_Session(spec, session_options, session_background, log, start_progress)
        created = Some(session)
        val consumer = Session.Consumer[Session.Phase](this.class_name) {
          case Session.Terminated(result) if !stopping =>
            Exn.capture {
              log.error_message(s"PIDE session ${quote(id)} terminated unexpectedly: " +
                PIDE_AI_Util.print_process_result(result))
            }
            terminated.change(_ => true)
            stop()
          case _ =>
        }
        session.phase_changed += consumer
        consumer.consume(session.phase) // session might have stopped already
        start_progress.expose_interrupt()
        log(s"PIDE session ${quote(id)} started")
        hooks.started(PIDE_AI_Sessions.Session_Event(session, start_progress))
        if (stopping || !session.is_ready) throw stopped_during_startup
        session
      }
      start_result match {
        case Exn.Res(session) =>
          started.fulfill(session)
          val progress = stop_request.guarded_access(request => request.map((_, request)))
          val result = shutdown(created, progress)
          if (terminated.value) PIDE_AI_Util.log_failure(log,
            s"Stopping terminated PIDE session ${quote(id)}", result)
          else Exn.release(result)
        case Exn.Exn(exn) =>
          val progress = stop_request.change_result { request =>
            val progress = request.getOrElse(start_progress)
            (progress, Some(progress))
          }
          val shutdown_result = shutdown(created, progress)
          val start_failure =
            if (Exn.is_interrupt(exn) && !caller_progress.stopped) stopped_during_startup
            else exn
          started.fulfill_result(Exn.Exn(PIDE_AI_Util.combined_failure(start_failure,
            List(shutdown_result).collect(Exn.the_exn))))
      }
    }
  }

  def start_session(
    base_options: Options,
    spec: PIDE_AI_Session_Spec,
    progress: PIDE_AI_Progress
  ): PIDE_AI_Session = {
    val entry = state.change_result { entries =>
      val session_id = spec.id.getOrElse(fresh_id(spec.logic, entries))
      if (session_id.isEmpty) error("Session id must not be empty")
      else if (entries.contains(session_id))
        error(s"Session id ${quote(session_id)} is already used")
      else {
        val entry = new Entry(session_id, base_options, spec.copy(id = Some(session_id)), progress)
        val forked = Task.fork("pide_ai_session_" + session_id, entry)
        (entry, entries + (session_id -> (entry, forked)))
      }
    }
    try { entry.started.join }
    catch { case exn: Throwable if Exn.is_interrupt(exn) => entry.stop(); throw exn }
  }

  def start_session_reporting_build_status(
    base_options: Options,
    spec: PIDE_AI_Session_Spec,
    progress: PIDE_AI_Progress
  ): PIDE_AI_Session = {
    val options = base_options ++ spec.options
    start_session(base_options, spec,
      PIDE_AI_Session.reporting_build_status_progress(progress,
        delay = PIDE_AI_Session.build_report_nodes_status_delay(options),
        long_running_threshold = PIDE_AI_Session.build_long_running_threshold(options),
        log = log))
  }

  def stop_sessions(session_ids: Option[List[String]], progress: PIDE_AI_Progress): List[String] = {
    val entries = state.value
    val selected = session_ids match {
      case None => entries.toList
      case Some(ids) =>
        val stoppable = entries.toList.filter { case (_, (entry, _)) => !entry.stopping }
        select(entries, "stoppable", stoppable, Some(ids.distinct))
    }
    for ((_, (entry, _)) <- selected) entry.request_stop(progress)
    PIDE_AI_Util.release_combined(selected.map { case (_, (_, forked)) =>
      if (forked.thread eq Thread.currentThread()) Exn.Res(()) else forked.join_result
    })
    selected.map(_._1)
  }
}
