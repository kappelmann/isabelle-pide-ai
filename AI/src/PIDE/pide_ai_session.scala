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
  def consolidate_delay(options: Options): Time = options.seconds("pide_ai_consolidate_delay")
  def prune_delay(options: Options): Time = options.seconds("pide_ai_prune_delay")
  def output_delay(options: Options): Time = options.seconds("pide_ai_output_delay")
  def range_context(options: Options): Int = options.int("pide_ai_range_context")
  def statistics_limit(options: Options): Int = options.int("pide_ai_session_statistics_limit")
  def build_encode_symbols(options: Options): Boolean =
    options.bool("pide_ai_build_encode_symbols")
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
    if (build_encode_symbols(options)) {
      val encoded = Symbols.recode_sessions(unicode_symbols = false,
        session_background.sessions_structure, List(session_background.session_name),
        requirements = true, base_logics = List(Isabelle_System.default_logic()))
      if (encoded.nonEmpty) progress.echo("Encoded symbols before build in: " +
        commas(encoded.map(path => quote(path.implode))))
    }
    val unicode_symbols = PIDE_AI_Resources.unicode_symbols(options)
    val results = Build.build(options,
      selection = Sessions.Selection.session(session_background.session_name),
      build_heap = true, dirs = spec.dirs, infos = session_background.infos,
      fresh_build = spec.fresh_build, no_build = spec.no_build,
      progress = new Recode_Progress(progress, unicode_symbols))
    if (spec.no_build && !results.ok)
      error("Missing or outdated heap image for session(s): " +
        commas_quote(results.unfinished))
    try { results.check }
    catch {
      case ERROR(msg) =>
        val errors = results.unfinished.flatMap(name =>
          Exn.result {
            results.store.try_open_database(name).toList.flatMap(db =>
              using(db)(results.store.read_errors(_, name)))
          } match {
            case Exn.Res(errors) => errors
            case Exn.Exn(exn) =>
              List(s"Cannot read errors of session ${quote(name)}: ${Exn.message(exn)}")
          })
        error(cat_lines(msg :: errors.map(Symbol.output(unicode_symbols, _))))
    }
    val session_options = base_options ++ (Options.Spec.eq("show_states", "true") ::
      Options.Spec.eq("editor_output_state", "false") :: spec.options)
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
    val resources = new PIDE_AI_Resources(options, session_background, log)
    progress.expose_interrupt()
    val session = new PIDE_AI_Session(id, spec.dirs, resources)
    try {
      val session_name = session_background.session_name
      progress.echo("Starting session " + session_name + " ...")
      Isabelle_Process.start(options, session, session_background,
        session.store.session_heaps(session_background, logic = session_name)).await_startup()
      progress.expose_interrupt()
      session
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
  _resources: PIDE_AI_Resources
) extends Session {
  override def resources: PIDE_AI_Resources = _resources
  override def session_options: Options = resources.options
  override def interactive: Boolean = true
  override def consolidate_delay: Time = PIDE_AI_Session.consolidate_delay(session_options)
  override def output_delay: Time = PIDE_AI_Session.output_delay(session_options)
  override def nodes_status_delay: Time = session_options.seconds("editor_output_delay")
  override def prune_delay: Time = PIDE_AI_Session.prune_delay(session_options)
  def poll_delay: Time = PIDE_AI_Session.poll_delay(session_options)
  def progress_delay: Time = PIDE_AI_Session.progress_delay(session_options)
  def long_running_threshold: Time = PIDE_AI_Session.long_running_threshold(session_options)

  def base_session: String = resources.current_background.session_name
  def directories(): List[Path] = Sessions.directories(dirs, Nil).map(_._2)

  private val lock = new Queue_Lock
  def with_lock[A](progress: PIDE_AI_Progress)(body: => A): A =
    lock.with_lock(progress,
      elapsed => "Awaiting session lock " + PIDE_AI_Progress.elapsed_text(elapsed),
      poll_delay, progress_delay)(body)

  private val statistics = Synchronized[Queue[Properties.T]](Queue.empty)
  private val statistics_consumer = Session.Consumer[Session.Runtime_Statistics](this.class_name) {
    case Session.Runtime_Statistics(props) => statistics.change { statistics =>
      val statistics1 = statistics.appended(props)
      if (statistics1.length > PIDE_AI_Session.statistics_limit(session_options)) {
        statistics1.dequeue._2
      }
      else statistics1
    }
  }
  runtime_statistics += statistics_consumer

  private val syslog_consumer = Session.Consumer[Prover.Output](this.class_name) { output =>
    if (output.is_system || output.is_stderr)
      resources.log(s"PIDE session ${quote(id)}: " +
        PIDE_AI_Util.content(List(output.message), resources.unicode_symbols))
  }
  syslog_messages += syslog_consumer

  def recent_runtime_statistics(): List[Properties.T] = statistics.value.toList

  val node_cache = new PIDE_AI_Node_Cache
  private val node_cache_consumer = Session.Consumer[Session.Commands_Changed](this.class_name) {
      changed => node_cache.changed(changed.nodes)
    }
  commands_changed += node_cache_consumer

  private val overlay_updates = Consumer_Thread.fork[() => Unit](s"pide_ai_overlays_$id",
    { e => e(); true }, daemon = true, log = resources.log)

  override def stop(): Process_Result = {
    runtime_statistics -= statistics_consumer
    syslog_messages -= syslog_consumer
    commands_changed -= node_cache_consumer
    val result = Exn.capture { super.stop() }
    PIDE_AI_Util.release_combined(List(result, Exn.capture { overlay_updates.shutdown() }))
    Exn.release(result)
  }

  def require_ready(): Unit =
    if (!is_ready)
      error(s"PIDE session ${quote(id)} is not ready. " +
        s"Its session phase is ${quote(phase.print)}")

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

  def tip_change: Document.Change = get_state().history.tip

  def await_stable(change: Document.Change, progress: PIDE_AI_Progress): Unit =
    await_while_ready(progress,
      elapsed => "Awaiting stable snapshot " + PIDE_AI_Progress.elapsed_text(elapsed),
      poll_delay, progress_delay) {
      val state = get_state()
      val unstable = state.history.undo_list.iterator.takeWhile(!state.is_stable(_))
      Option.when(!unstable.contains(change))(())
    }

  def node_snapshot(node_name: Document.Node.Name): Document.Snapshot =
    switch(snapshot(), node_name)

  def switch(
    snapshot: Document.Snapshot,
    node_name: Document.Node.Name
  ): Document.Snapshot = {
    val new_snapshot = snapshot.switch(node_name)
    if (PIDE_AI_Nodes.is_loaded(new_snapshot.version.nodes, node_name)) new_snapshot
    else error("No PIDE snapshot available for " +
      quote(resources.print_node_name(node_name, None)))
  }

  def tip_version(progress: PIDE_AI_Progress): Document.Version = {
    progress.expose_interrupt()
    val version = get_state().history.tip.version
    await_while_ready(progress,
      elapsed => "Awaiting current document version " + PIDE_AI_Progress.elapsed_text(elapsed),
      poll_delay, progress_delay)(
        version.peek.map(Exn.release))
  }

  def node_source(
    snapshot: Document.Snapshot,
    node_name: Document.Node.Name
  ): String =
    try { switch(snapshot, node_name).node.source }
    catch { case ERROR(_) => resources.read_file_content(node_name) }

  def update(nodes: Document.Nodes, edits: List[Document.Edit_Text]): Document.Change = {
    if (edits.nonEmpty) {
      val loaded_blobs = nodes.iterator.flatMap { (name, node) =>
        node.get_blob.filter(_ => PIDE_AI_Nodes.is_loaded(nodes, name)).map(name -> _) }
      val edited_blobs = edits.collect { case (name, Document.Node.Blob(blob)) => name -> blob }
      update(Document.Blobs(loaded_blobs.toMap ++ edited_blobs), edits)
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
        val nodes = tip_version(progress).nodes
        val node = nodes(node_name)
        if (PIDE_AI_Nodes.is_loaded(nodes, node_name))
          update(nodes, List(node_name -> PIDE_AI_Document_Model.node_perspective(node)
            .copy(overlays = change(node.perspective.overlays))))
        else if (require_theory)
          error(quote(resources.print_node_name(node_name, None)) +
            " is not a dynamically loaded theory")
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
        PIDE_AI_Document_Model.make(this, name, version.nodes(name),
          resources.read_file_content(name), visible_lines,
          PIDE_AI_Session.range_context(session_options))
      }
      val other_edits = if (!hide_others) Nil
        else PIDE_AI_Document_Model.hide_edits(version.nodes, nodes.map(_._1).toSet)
      (models1, update(version.nodes, other_edits ::: models1.flatMap(_.edits)))
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
        node_names.filter(PIDE_AI_Nodes.is_loaded(nodes, _)).map(_ -> None),
        hide_others = false, progress = progress)
    }

  def sync_resolve(
    node_names: List[Document.Node.Name],
    progress: PIDE_AI_Progress
  ): Document.Change =
    with_lock(progress) {
      sync(node_names, progress)
      resolve_dependencies(node_names, progress)
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
    val (texts, change) = read_update(List(node_name -> Some(visible_lines)), hide_others, progress)
    if (await_stable_before_resolve) await_stable(change, progress)
    resolve_dependencies(List(node_name), progress)
    texts(node_name)
  }

  def resolve_dependencies(
    node_names: List[Document.Node.Name],
    progress: PIDE_AI_Progress
  ): Document.Change = {
    @tailrec def loop(seen: Set[Document.Node.Name], node_names: List[Document.Node.Name]): Unit = {
      val names =
        resources.nodes_to_load(conditions, tip_version(progress).nodes, node_names, seen, progress)
      if (names.nonEmpty) {
        read_update(names.map(_ -> Some(Nil)), hide_others = false, progress = progress)
        loop(seen ++ names, names)
      }
    }
    loop(Set.empty, node_names)
    tip_change
  }

  def unload(
    node_names: List[Document.Node.Name],
    progress: PIDE_AI_Progress,
    cwd: Option[Path]
  ): (List[Document.Node.Name], Document.Change) = {
    with_lock(progress) {
      val nodes = tip_version(progress).nodes
      val unloaded = resources.nodes_to_unload(nodes, node_names)
      for (name <- node_names) {
        val loading = nodes.commands_loading(name).map(_.node_name).filterNot(unloaded.contains)
        if (loading.nonEmpty) {
          val theories = commas_quote(loading.map(resources.print_node_name(_, cwd)))
          error(s"Node ${quote(resources.print_node_name(name, cwd))} is loaded by $theories. " +
            s"Unload $theories instead.")
        }
      }
      update(nodes, unloaded.flatMap(name =>
        PIDE_AI_Document_Model.unload_edits(this, name, nodes(name))))
      (unloaded, update(tip_version(progress).nodes, unloaded.filter(_.is_theory).map(
        _ -> Document.Node.Thy(Resources.Thy.empty))))
    }
  }
}
