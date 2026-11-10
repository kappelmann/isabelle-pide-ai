/*  Title:      AI/src/PIDE/pide_ai_session.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.ai

import isabelle._
import isabelle.symbols.Symbols

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object PIDE_AI_Session {
  def progress_delay(options: Options): Time = options.seconds("pide_ai_progress_delay")
  def poll_delay(options: Options): Time = options.seconds("pide_ai_poll_delay")
  def range_context(options: Options): Int = options.int("pide_ai_range_context")
  def statistics_limit(options: Options): Int = options.int("pide_ai_session_statistics_limit")
  def unicode_symbols(options: Options): Boolean = options.bool("pide_ai_unicode_symbols")
  def unicode_symbols_disk(options: Options): Boolean =
    options.bool("pide_ai_unicode_symbols_disk")
  def build_long_running_threshold(options: Options): Time =
    options.seconds("pide_ai_build_long_running_threshold")
  val long_running_threshold_option: String = "pide_ai_long_running_threshold"
  def long_running_threshold(options: Options): Time =
    options.seconds(long_running_threshold_option)
  def build_report_nodes_status_delay(options: Options): Time =
    options.seconds("pide_ai_build_report_nodes_status_delay")

  def reporting_build_status_progress(
    progress: PIDE_AI_Progress,
    delay: Time,
    long_running_threshold: Time,
    log: Logger
  ): PIDE_AI_Progress = {
    val report = new Throttle[Progress.Nodes_Status](delay, log)(status =>
      progress.output(
        PIDE_AI_Progress.progress_long_running_output(long_running_threshold)(status)))
    new Reporting_Progress(progress, report_nodes_status = status =>
      if (status.domain.nonEmpty) report(status))
  }

  def build(
    base_options: Options,
    spec: PIDE_AI_Session_Spec,
    progress: PIDE_AI_Progress
  ): (Options, Sessions.Background) = {
    val options = base_options ++ spec.options
    val session_background = Sessions.background(options, spec.logic,
      progress = new Silent_Progress(progress),
      dirs = spec.dirs, session_ancestor = spec.session_ancestor,
      session_requirements = spec.session_requirements).check_errors
    val encoded = Symbols.recode_sessions(unicode_symbols = false,
      session_background.sessions_structure, List(session_background.session_name))
    if (encoded.nonEmpty) progress.echo("Encoded symbols before build in: " +
      commas(encoded.map(path => quote(path.implode))))
    Build.build(options, selection = Sessions.Selection.session(session_background.session_name),
      build_heap = true, dirs = spec.dirs, infos = session_background.infos,
      fresh_build = spec.fresh_build, no_build = spec.no_build,
      progress = new Recode_Progress(progress, unicode_symbols(options))).check
    val session_options = base_options ++ (Options.Spec.eq("show_states", "true") ::
      Options.Spec.eq("editor_output_state", "false") :: spec.options :::
      List(Options.Spec.eq("headless_consolidate_delay",
        options.check_name("pide_ai_consolidate_delay").value)))
    (session_options, session_background)
  }

  def apply(
    spec: PIDE_AI_Session_Spec,
    options: Options,
    session_background: Sessions.Background,
    log: Logger,
    progress: PIDE_AI_Progress
  ): PIDE_AI_Session = {
    val id = spec.id.getOrElse(error("Missing session id"))
    val resources = PIDE_AI_Headless.Resources(options, session_background, log)
    progress.expose_interrupt()
    val session = resources.start_session(progress = progress)
    try {
      progress.expose_interrupt()
      new PIDE_AI_Session(id = id, dirs = spec.dirs, session = session)
    }
    catch {
      case exn: Throwable =>
        val stop_failure = Exn.capture(session.stop()) match {
          case Exn.Res(process_result) =>
            Option.when(!process_result.ok)(PIDE_AI_Util.print_process_result(process_result))
          case Exn.Exn(stop_exn) => Some(Exn.message(stop_exn))
        }
        stop_failure match {
          case None => throw exn
          case Some(message) => throw ERROR(cat_lines(List(
            Exn.message(exn),
            s"Failed to stop partially started PIDE session ${quote(id)}: " + message)))
        }
    }
  }
}

class PIDE_AI_Session private(
  val id: String,
  val dirs: List[Path],
  val session: PIDE_AI_Headless.Session
) {
  def resources: PIDE_AI_Headless.Resources = session.resources
  def options: Options = resources.options
  val unicode_symbols: Boolean = PIDE_AI_Session.unicode_symbols(options)
  def disk_text(path: Path, text: String): String =
    if (Symbols.supports_isabelle_symbols(path)) {
      Symbol.output(PIDE_AI_Session.unicode_symbols_disk(options), text)
    }
    else text
  def pide_text(path: Path, text: String): String =
    if (Symbols.supports_isabelle_symbols(path)) Symbol.output(unicode_symbols, text) else text

  def base_session: String = resources.current_background.session_name
  def directories(): List[Path] = Sessions.directories(dirs, Nil).map(_._2)

  private val lock = new Queue_Lock
  def with_lock[A](progress: PIDE_AI_Progress)(body: => A): A =
    lock.with_lock(progress,
      elapsed => "Awaiting session lock " + PIDE_AI_Progress.elapsed_text(elapsed),
      PIDE_AI_Session.poll_delay(options), PIDE_AI_Session.progress_delay(options))(body)

  private val statistics = Synchronized[Queue[Properties.T]](Queue.empty)
  private val statistics_consumer = Session.Consumer[Session.Runtime_Statistics](this.class_name) {
    case Session.Runtime_Statistics(props) =>
      statistics.change { statistics =>
        val statistics1 = statistics.appended(props)
        if (statistics1.length > PIDE_AI_Session.statistics_limit(options)) {
          statistics1.dequeue._2
        }
        else statistics1
      }
  }
  session.runtime_statistics += statistics_consumer

  private val syslog_consumer = Session.Consumer[Prover.Output](this.class_name) { output =>
    if (output.is_system || output.is_stderr)
      resources.log(s"PIDE session ${quote(id)}: " + XML.content(output.message))
  }
  session.syslog_messages += syslog_consumer

  def runtime_statistics(): List[Properties.T] = statistics.value.toList

  private val overlay_updates = Consumer_Thread.fork[() => Unit](s"pide_ai_overlays_$id",
    { e => e(); true }, daemon = true, log = resources.log)

  def stop(): Process_Result = {
    session.runtime_statistics -= statistics_consumer
    session.syslog_messages -= syslog_consumer
    val result = Exn.capture { session.stop() }
    PIDE_AI_Util.release_combined(List(result, Exn.capture { overlay_updates.shutdown() }))
    Exn.release(result)
  }

  def source_node_name(source_name: String): Document.Node.Name =
    PIDE_AI_Origin.source_node_name(resources, session.store, source_name)

  def node_name(s: String, cwd: Option[Path]): Document.Node.Name =
    PIDE_AI_Origin.node_name(resources, s, cwd)

  def print_origin(node_name: Document.Node.Name, cwd: Option[Path]): String =
    PIDE_AI_Origin.print(resources, node_name, cwd)

  private def is_base_session_theory(node_name: Document.Node.Name): Boolean =
    PIDE_AI_Nodes.is_base_session_theory(resources, node_name)

  def snapshot(): Document.Snapshot = session.snapshot()

  def require_ready(): Unit =
    if (!session.is_ready)
      error(s"PIDE session ${quote(id)} is not ready. " +
        s"Its session phase is ${quote(session.phase.print)}")

  def await_while_ready[A](
    progress: PIDE_AI_Progress,
    message: Time => JSON.T,
    delay: => Time,
    progress_delay: Time
  )(value: => Option[A]): A =
    PIDE_AI_Progress.await_delay(progress, message, delay, progress_delay) {
      require_ready()
      value
    }

  def tip_change: Document.Change = session.get_state().history.tip

  def await_stable(change: Document.Change, progress: PIDE_AI_Progress): Unit =
    await_while_ready(progress,
      elapsed => "Awaiting stable snapshot " + PIDE_AI_Progress.elapsed_text(elapsed),
      PIDE_AI_Session.poll_delay(options), PIDE_AI_Session.progress_delay(options)) {
      val state = session.get_state()
      val unstable = state.history.undo_list.iterator.takeWhile(!state.is_stable(_))
      Option.when(!unstable.contains(change))(())
    }

  def node_snapshot(node_name: Document.Node.Name): Document.Snapshot =
    switch(session.snapshot(), node_name)

  def switch(
    snapshot: => Document.Snapshot,
    node_name: Document.Node.Name
  ): Document.Snapshot = {
    if (is_base_session_theory(node_name))
      session.read_theory(node_name.theory, unicode_symbols = unicode_symbols)
    else {
      val snapshot1 = snapshot
      val new_snapshot = (if (is_base_session_theory(snapshot1.node_name)) session.snapshot()
        else snapshot1).switch(node_name)
      if (PIDE_AI_Nodes.is_loaded_dynamic(new_snapshot.node)) new_snapshot
      else error(s"No PIDE snapshot available for ${quote(print_origin(node_name, None))}")
    }
  }

  def tip_version(progress: PIDE_AI_Progress): Document.Version = {
    progress.expose_interrupt()
    val version = session.get_state().history.tip.version
    await_while_ready(progress,
      elapsed => "Awaiting current document version " + PIDE_AI_Progress.elapsed_text(elapsed),
      PIDE_AI_Session.poll_delay(options), PIDE_AI_Session.progress_delay(options))(
        version.peek.map(Exn.release))
  }

  def read_file_content(node_name: Document.Node.Name): String =
    resources.make_theory_content(node_name).getOrElse(
      pide_text(node_name.path, Line.normalize(File.read(node_name.path))))

  def write_file_content(path: Path, text: String): Unit =
    File.write(path, disk_text(path, Line.normalize(text)))

  def node_source(
    snapshot: Document.Snapshot,
    node_name: Document.Node.Name
  ): String =
    try { switch(snapshot, node_name).node.source }
    catch { case ERROR(_) => read_file_content(node_name) }

  def update(edits: List[Document.Edit_Text]): Document.Change = {
    if (edits.nonEmpty) {
      val blobs = edits.collect { case (name, Document.Node.Blob(blob)) => name -> blob }
      session.update(Document.Blobs(blobs.toMap), edits)
    }
    tip_change
  }

  private def edit_overlays(
    node_name: Document.Node.Name,
    change: Document.Node.Overlays => Document.Node.Overlays,
    require_theory: Boolean,
    progress: PIDE_AI_Progress
  ): Future[Unit] = {
    def edit(): Unit = {
      require_ready()
      with_lock(progress) {
        val node = tip_version(progress).nodes(node_name)
        if (PIDE_AI_Nodes.is_loaded_dynamic(node))
          update(List(node_name -> PIDE_AI_Document_Model.node_perspective(node)
            .copy(overlays = change(node.perspective.overlays))))
        else if (require_theory)
          error(s"${quote(print_origin(node_name, None))} is not a dynamically loaded theory")
      }
    }
    val result = Future.promise[Unit]
    def done(res: Exn.Result[Unit]): Unit = {
      res match {
        case Exn.Exn(exn) if !Exn.is_interrupt(exn) => progress.echo_error_message(Exn.message(exn))
        case _ =>
      }
      result.fulfill_result(res)
    }
    Exn.capture { require_ready(); overlay_updates.send(() => done(Exn.capture(edit()))) } match {
      case failure @ Exn.Exn(_) => done(failure)
      case Exn.Res(_) =>
    }
    result
  }

  def insert_overlay(
    command: Command,
    fn: String,
    args: List[String],
    progress: PIDE_AI_Progress
  ): Future[Unit] =
    edit_overlays(command.node_name, _.insert(command, fn, args), require_theory = true, progress)

  def remove_overlay(
    command: Command,
    fn: String,
    args: List[String],
    progress: PIDE_AI_Progress
  ): Future[Unit] =
    edit_overlays(command.node_name, _.remove(command, fn, args), require_theory = false, progress)

  def editor(progress: PIDE_AI_Progress): PIDE_AI_Editor = new PIDE_AI_Editor(this, progress)

  def read_update(
    nodes: List[(Document.Node.Name, Option[List[(Int, Option[Int])]])],
    hide_others: Boolean,
    progress: PIDE_AI_Progress
  ): (Map[Document.Node.Name, String], Document.Change) = {
    val (models, change) = with_lock(progress) {
      val version = tip_version(progress)
      val models1 = nodes.map { (name, visible_lines) =>
        PIDE_AI_Document_Model.make(session, name, version.nodes(name), read_file_content(name),
          visible_lines, PIDE_AI_Session.range_context(options))
      }
      val other_edits = if (!hide_others) Nil
        else PIDE_AI_Document_Model.hide_edits(version.nodes, nodes.map(_._1).toSet)
      (models1, update(other_edits ::: models1.flatMap(_.edits)))
    }
    (models.map(model => model.node_name -> model.text).toMap, change)
  }

  def sync(
    node_names: List[Document.Node.Name],
    progress: PIDE_AI_Progress
  ): (Map[Document.Node.Name, String], Document.Change) =
    with_lock(progress) {
      val nodes = tip_version(progress).nodes
      read_update(
        node_names.filter(name => PIDE_AI_Nodes.is_loaded_dynamic(nodes(name))).map(_ -> None),
        hide_others = false, progress = progress)
    }

  def sync_resolve(
    node_names: List[Document.Node.Name],
    progress: PIDE_AI_Progress
  ): Document.Change =
    with_lock(progress) {
      sync(node_names, progress)
      resolve_dependencies(node_names, progress)
      tip_change
    }

  def sync_dependencies_resolve(
    node_names: List[Document.Node.Name],
    progress: PIDE_AI_Progress
  ): Document.Change =
    with_lock(progress) {
      sync_resolve(PIDE_AI_Dependencies.known_dependencies(resources,
        tip_version(progress).nodes, node_names), progress)
    }

  def sync_loaded_resolve(
    node_names: List[Document.Node.Name],
    progress: PIDE_AI_Progress
  ): Document.Change =
    with_lock(progress) {
      val (texts, _) = sync(node_names, progress)
      resolve_dependencies(texts.keys.toList, progress)
      tip_change
    }

  def read_update_resolve(
    node_name: Document.Node.Name,
    visible_lines: List[(Int, Option[Int])],
    await_stable_before_resolve: Boolean,
    hide_others: Boolean,
    progress: PIDE_AI_Progress
  ): String = {
    if (is_base_session_theory(node_name)) node_snapshot(node_name).node.source
    else {
      val (texts, change) =
        read_update(List(node_name -> Some(visible_lines)), hide_others, progress)
      if (await_stable_before_resolve) await_stable(change, progress)
      resolve_dependencies(List(node_name), progress)
      texts(node_name)
    }
  }

  def resolve_dependencies(
    node_names: List[Document.Node.Name],
    progress: PIDE_AI_Progress
  ): Unit = {
    @tailrec def loop(seen: Set[Document.Node.Name], node_names: List[Document.Node.Name]): Unit = {
      val names = PIDE_AI_Dependencies.unloaded_dependencies(session,
        tip_version(progress).nodes, node_names, seen, progress)
      if (names.nonEmpty) {
        read_update(names.map(_ -> Some(Nil)), hide_others = false, progress = progress)
        loop(seen ++ names, names)
      }
    }
    loop(Set.empty, node_names)
  }

  def unload(
    node_names: List[Document.Node.Name],
    progress: PIDE_AI_Progress
  ): (List[Document.Node.Name], Document.Change) = {
    for (name <- node_names if is_base_session_theory(name))
      error(s"Cannot unload base session theory ${quote(print_origin(name, None))}")
    with_lock(progress) {
      val nodes = tip_version(progress).nodes
      val unloaded = PIDE_AI_Dependencies.removable_names(resources, nodes, node_names)
      (unloaded, update(unloaded.flatMap(name =>
        PIDE_AI_Document_Model.unload_edits(session, name, nodes(name)))))
    }
  }
}
