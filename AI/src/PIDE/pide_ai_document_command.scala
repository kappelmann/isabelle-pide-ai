/*  Title:      AI/src/PIDE/pide_ai_document_command.scala
    Author:     Kevin Kappelmann

Command utilities.
*/

package isabelle.pide.ai

import isabelle._

object PIDE_AI_Document_Command {
  def snapshot_contains(snapshot: Document.Snapshot, cmd: Command): Boolean =
    snapshot.command_node(cmd).commands.contains(cmd)

  def self_id(
    state: Document.State,
    cmd: Command
  )(id: Document_ID.Generic): Boolean =
    id == cmd.id || state.lookup_id(id).exists(_.command.id == cmd.id)

  def elem_positions(
    snapshot: Document.Snapshot,
    cmd: Command,
    elem: XML.Elem
  ): Set[Text.Range] = {
    val chunk_name = if (snapshot.node_name.is_theory) Symbol.Text_Chunk.Default
      else Symbol.Text_Chunk.File(snapshot.node_name.node)
    cmd.chunks.get(chunk_name) match {
      case None => Set.empty
      case Some(chunk) =>
        cmd.message_positions(self_id(snapshot.state, cmd), chunk_name, chunk, elem)
    }
  }

  private val here: String = Symbol.decode("\\<^here>")

  def resolve_positions(
    snapshot: Document.Snapshot,
    cmd: Command,
    body: XML.Body,
    cwd: Option[Path]
  ): XML.Body = {
    def same_file(file: String): Boolean = file == snapshot.node_name.node ||
      PIDE_AI_Util.absolute_file(file).exists(
        PIDE_AI_Util.absolute_file(snapshot.node_name.node).contains)
    def line_text(line: Int, file: String): String =
      "(line " + line +
      if_proper(!same_file(file), " of " + quote(PIDE_AI_Util.print_file(file, cwd))) + ")"
    body.flatMap {
      case XML.Elem(Markup(Markup.POSITION, Position.Item_Id(id, range)), List(XML.Text(text)))
      if text == here => if (self_id(snapshot.state, cmd)(id)) Nil
        else snapshot.find_command_position(id, range.start)
          .map(pos => XML.Text(" " + line_text(pos.line1, pos.name))).toList
      case XML.Elem(markup @ Markup(Markup.POSITION, props @ Position.Line(line)), _)
      if Position.File.unapply(props).exists(same_file) =>
        List(XML.Elem(markup, XML.string(line_text(line, snapshot.node_name.node))))
      case XML.Elem(markup, body1) =>
        List(XML.Elem(markup, resolve_positions(snapshot, cmd, body1, cwd)))
      case text => List(text)
    }
  }

  def is_ignorable(
    snapshot: Document.Snapshot,
    cmd: Command,
    status: => Document_Status.Command_Status
  ): Boolean =
    cmd.is_ignored &&
    !PIDE_AI_Command_Status.statuses(status).exists(PIDE_AI_Command_Status.problematic) &&
    snapshot.command_results(cmd).is_empty
  def is_ignorable(snapshot: Document.Snapshot, cmd: Command): Boolean =
    is_ignorable(snapshot, cmd, PIDE_AI_Command_Status.command_status(snapshot, cmd))
  def is_ignorable_for_good(snapshot: Document.Snapshot, cmd: Command): Boolean =
    is_ignorable(snapshot, cmd) &&
    PIDE_AI_Command_Status.command_status(snapshot, cmd).is_terminated

  def is_terminated_slow(
    status: Document_Status.Command_Status,
    threshold: Time,
    now: Date
  ): Boolean = status.is_terminated && status.timings.sum(now) >= threshold

  val terminated_timings: PIDE_AI_Node_Cache.Data[List[(Command, Time)]] =
    PIDE_AI_Node_Cache.Data { snapshot =>
      val now = Date.now()
      snapshot.node.commands.iterator.flatMap { cmd =>
        val status = PIDE_AI_Command_Status.command_status(snapshot, cmd)
        Option.when(status.is_terminated)(cmd -> status.timings.sum(now))
      }.toList.sortBy(-_._2.ms)
    }

  object Selection {
    def apply(snapshot: Document.Snapshot, range: Option[Text.Range]): Selection =
      PIDE_AI_Nodes.node_command(snapshot.version.nodes, snapshot.node_name) match {
        case Some(cmd) =>
          node_command(cmd, PIDE_AI_Util.restrict_text_range(snapshot.node.source, range))
        case None => commands(
          range.fold(snapshot.node.command_iterator())(snapshot.node.command_iterator(_))
            .map { (cmd, start) =>
              cmd -> (PIDE_AI_Util.intersect_range(cmd.range + start, range) - start)
            }.toList)
      }

    def nodes(
      session: PIDE_AI_Session,
      snapshot: Document.Snapshot,
      names: List[Document.Node.Name]
    ): List[(Document.Snapshot, Selection)] =
      names.map { name =>
        val node_snapshot = session.switch(snapshot, name)
        node_snapshot -> Selection(node_snapshot, None)
      }

    def running(
      session: PIDE_AI_Session,
      view: PIDE_AI_Node_Cache.View,
      names: List[Document.Node.Name]
    ): List[(Document.Snapshot, Selection)] =
      nodes(session, view.snapshot, PIDE_AI_Theory_Status.Statuses.running(view, names))

    def terminated_slow(
      session: PIDE_AI_Session,
      view: PIDE_AI_Node_Cache.View,
      names: List[Document.Node.Name],
      long_running_threshold: Time
    ): List[(Document.Snapshot, Selection)] = {
      val slow = names.flatMap(name => proper_list(view(terminated_timings, name)
        .takeWhile(_._2 >= long_running_threshold).map(_._1)).map(name -> _.toSet))
      val slow_cmds = slow.toMap
      nodes(session, view.snapshot, slow.map(_._1)).map((snapshot, selection) =>
        snapshot -> selection.filter((cmd, _) => slow_cmds(snapshot.node_name)(cmd)))
    }
  }

  enum Selection {
    case node_command(cmd: Command, range: Text.Range)
     // commands intersecting range, with the intersection relative to their start
    case commands(cmds: List[(Command, Text.Range)])

    def parts: List[(Command, Text.Range)] =
      this match {
        case Selection.node_command(cmd, range) => List(cmd -> range)
        case Selection.commands(cmds) => cmds
      }

    def filter(pred: ((Command, Text.Range)) => Boolean): Selection =
      this match {
        case Selection.node_command(cmd, range) =>
          if (pred(cmd -> range)) this else Selection.commands(Nil)
        case Selection.commands(cmds) => Selection.commands(cmds.filter(pred))
      }
  }

  object State {
    enum Detail { case errors, warnings, infos, goals, src, types, facts, sendbacks, markup }

    object Detail {
      def of(kind: PIDE_AI_Message.Kind): Detail =
        kind match {
          case PIDE_AI_Message.Kind.state => goals
          case PIDE_AI_Message.Kind.error => errors
          case PIDE_AI_Message.Kind.warning => warnings
          case PIDE_AI_Message.Kind.writeln | PIDE_AI_Message.Kind.information |
            PIDE_AI_Message.Kind.tracing => infos
        }

      def kinds(detail: Detail): List[PIDE_AI_Message.Kind] =
        PIDE_AI_Message.Kind.values.toList.filter(of(_) == detail)
    }
  }

  sealed case class State private[PIDE_AI_Document_Command](
    snapshot: Document.Snapshot,
    cmd: Command,
    range: Text.Range,
    messages: List[PIDE_AI_Message]
  ) {
    lazy val command_status: Document_Status.Command_Status =
      PIDE_AI_Command_Status.command_status(snapshot, cmd)
    private lazy val range_command_status =
      PIDE_AI_Command_Status.range_command_status(snapshot, range)
    lazy val status: List[PIDE_AI_Command_Status] =
      PIDE_AI_Command_Status.statuses(command_status).filter {
        case PIDE_AI_Command_Status.warned => range_command_status.is_warned
        case PIDE_AI_Command_Status.failed => range_command_status.is_failed
        case _ => true
      }
    lazy val timing_ms: Long = command_status.timings.sum(Date.now()).ms
    def is_long_running(long_running_threshold: Time): Boolean =
      timing_ms >= long_running_threshold.ms
    private lazy val messages_by_kind = messages.groupBy(_.kind)
    def messages_of(kind: PIDE_AI_Message.Kind): List[PIDE_AI_Message] =
      messages_by_kind.getOrElse(kind, Nil)
    lazy val errors = messages_of(PIDE_AI_Message.Kind.error)
    lazy val bad: List[Text.Info[XML.Elem]] = PIDE_AI_Markup.bad(snapshot, range)
      .filterNot(info => errors.nonEmpty && XML.content(info.info.body).isEmpty)
    private def bad_json(doc: Line.Document, unicode_symbols: Boolean): List[JSON.Object.T] =
      bad.map { info => JSON_Object.flatten(
        proper_string(PIDE_AI_Util.formatted_content(info.info.body, unicode_symbols))
          .map("message" -> _),
        Some("src" -> PIDE_AI_Util.numbered_lines(info.range.substring(snapshot.node.source),
          doc.position(info.range.start).line1)))
      }

    def json(
      doc: Line.Document,
      details: Set[State.Detail],
      unicode_symbols: Boolean,
      cwd: Option[Path]
    ): JSON.Object.T = {
      val (start_line, end_line) = PIDE_AI_Util.range_lines(doc, range)
      val entries: List[Option[JSON.Object.Entry]] = List(
        proper_list(status.map(_.name)).map("status" -> _),
        Option.when(timing_ms > 0)("timing_ms" -> timing_ms),
        Some(
          if (details(State.Detail.src))
            "src" -> PIDE_AI_Util.numbered_lines(
              range.substring(snapshot.node.source).stripLineEnd, start_line)
          else "lines" -> PIDE_AI_Util.line_range(start_line, end_line)),
        proper_list(bad_json(doc, unicode_symbols)).map(Markup.BAD -> _)) :::
        PIDE_AI_Message.Messages.json(messages_by_kind,
          PIDE_AI_Message.Kind.values.toList.filter(kind =>
            details(State.Detail.of(kind))), unicode_symbols, cwd) :::
        List(
          Option.when(details(State.Detail.types))(
            proper_list(PIDE_AI_Markup.types_json(snapshot, range, unicode_symbols))
              .map("types" -> _)).flatten,
          Option.when(details(State.Detail.facts))(
            proper_list(PIDE_AI_Markup.facts_json(snapshot, range, unicode_symbols))
              .map("facts" -> _)).flatten,
          Option.when(details(State.Detail.sendbacks))(
            proper_list(messages.flatMap(_.sendbacks(unicode_symbols))).map("sendbacks" -> _))
            .flatten,
          Option.when(details(State.Detail.markup))(
            "markup" -> PIDE_AI_Markup.markup_json(snapshot, range, Markup.Elements.full,
              unicode_symbols)))
      JSON_Object.flatten(entries)
    }
  }

  object States {
    def cmds_key(name: String): String = "cmds_" + name

    def command_spans(
      snapshot: Document.Snapshot,
      theory_cmd: Command,
      range: Option[Text.Range]
    ): Iterator[State] = {
      val node_range = PIDE_AI_Util.restrict_text_range(snapshot.node.source, range)
      proper_list(snapshot.command_spans(node_range).map(_.range)).getOrElse(List(node_range))
        .iterator.map { span_range =>
          val restricted = PIDE_AI_Util.intersect_range(span_range, range)
          State(snapshot, theory_cmd, restricted,
            Rendering.text_messages(snapshot, restricted).flatMap(info =>
              PIDE_AI_Message.Kind.of(info.info).map(kind =>
                PIDE_AI_Message(kind, info.info, List(info.range), snapshot,
                  Some(theory_cmd)))))
        }
    }

    def apply(snapshot: Document.Snapshot, selection: Selection): Iterator[State] = {
      val states = selection match {
        case Selection.node_command(cmd, range) => if (snapshot.node_name.is_theory)
            command_spans(snapshot, cmd, Some(range))
          else
            Iterator(State(snapshot, cmd, range, PIDE_AI_Message.Messages(snapshot, cmd, 0, range)))
        case Selection.commands(cmds) =>
          val starts = snapshot.node.command_iterator().toMap
          for ((cmd, part) <- cmds.iterator; start <- starts.get(cmd)) yield {
            val range = part + start
            State(snapshot, cmd, range, PIDE_AI_Message.Messages(snapshot, cmd, start, range))
          }
      }
      states.filterNot(state => is_ignorable(snapshot, state.cmd))
    }

    def apply(snapshot: Document.Snapshot, range: Option[Text.Range]): Iterator[State] =
      apply(snapshot, Selection(snapshot, range))

    sealed case class Show(
      message_kinds: List[PIDE_AI_Message.Kind],
      terminated_slow: Boolean,
      summary_limit: Option[Int],
      snippet_lines: Int,
      details: Set[State.Detail],
      problematic_limit: Option[Int],
      unproblematic_limit: Option[Int])

    def counted_summary(count: Int)(summary: => JSON.Object.T): Option[JSON.Object.T] =
      Option.when(count > 0)(JSON_Object("count" -> count) ++ summary)

    def state_item_json(
      state: State,
      doc: Line.Document,
      snippet_lines: Int,
      long_running_threshold: Time
    ): JSON.Object.T =
      JSON_Object("snippets" -> List(PIDE_AI_Util.snippet(doc, state.range, snippet_lines))) ++
      JSON.optional("timing_ms" ->
        Option.when(state.is_long_running(long_running_threshold))(state.timing_ms))

    def message_item_json(
      ranges: List[Text.Range],
      message: String,
      doc: Line.Document,
      snippet_lines: Int
    ): JSON.Object.T =
      JSON_Object("snippets" -> ranges.map(PIDE_AI_Util.snippet(doc, _, snippet_lines))) ++
      JSON.optional("message" -> proper_string(message))

    def terminated_slow_json(
      states: List[State],
      doc: Line.Document,
      snippet_lines: Int,
      long_running_threshold: Time
    ): JSON.Object.T =
      JSON_Object("items" -> states.sortBy(_.timing_ms)(Ordering[Long].reverse).map(state =>
        JSON_Object("status" -> state.status.map(_.name)) ++
        state_item_json(state, doc, snippet_lines, long_running_threshold)))

    def running_json(
      states: List[State],
      doc: Line.Document,
      snippet_lines: Int,
      long_running_threshold: Time
    ): JSON.Object.T = {
      val (long_running, short_running) =
        states.partition(_.is_long_running(long_running_threshold))
      JSON_Object.flatten(
        Option.when(long_running.nonEmpty)("long_running" ->
          long_running.map(state_item_json(_, doc, snippet_lines, long_running_threshold))),
        Option.when(short_running.nonEmpty)("short_running_lines" ->
          short_running.map(state => PIDE_AI_Util.range_lines(doc, state.range))
            .map(PIDE_AI_Util.line_range)))
    }

    def json(
      states: List[State],
      show: Show,
      doc: Line.Document,
      long_running_threshold: Time,
      unicode_symbols: Boolean,
      cwd: Option[Path]
    ): JSON.Object.T = {
      def reported_statuses(state: State): List[PIDE_AI_Command_Status] =
        state.status.filter(_.message_kind.forall(show.message_kinds.contains))
      def reported_messages(state: State): List[PIDE_AI_Message] =
        show.message_kinds.flatMap(state.messages_of)

      val status_counts = states.flatMap(reported_statuses).groupMapReduce(identity)(_ => 1)(_ + _)
        .withDefaultValue(0)
      val message_counts = states.flatMap(reported_messages).groupMapReduce(_.kind)(_ => 1)(_ + _)
        .withDefaultValue(0)
      val running = states.filter(_.status.contains(PIDE_AI_Command_Status.running))
      val terminated = states.count(_.command_status.is_terminated)

      val items = {
        val all = states.iterator.flatMap { state =>
          lazy val item = state_item_json(state, doc, show.snippet_lines, long_running_threshold)
          val status_items = reported_statuses(state).filter(status =>
            PIDE_AI_Command_Status.problematic(status) &&
            status.message_kind.forall(state.messages_of(_).isEmpty))
            .map(status => cmds_key(status.name) -> item)
          val bad_items = state.bad.map(info => Markup.BAD ->
            message_item_json(List(info.range),
              PIDE_AI_Util.formatted_content(info.info.body, unicode_symbols), doc,
              show.snippet_lines))
          val message_items = reported_messages(state).map(msg =>
            PIDE_AI_Message.Messages.key(msg.kind) ->
              message_item_json(msg.ranges, msg.text(unicode_symbols, cwd), doc,
                show.snippet_lines))
          status_items ::: bad_items ::: message_items
        }
        show.summary_limit.fold(all)(all.take).toList.groupMap(_._1)(_._2)
      }

      def summary_entry(
        key: String,
        count: Int
      )(summary: => JSON.Object.T): Option[JSON.Object.Entry] =
        counted_summary(count)(summary).map(key -> _)
      def items_entry(
        key: String,
        count: Int,
        without: Option[PIDE_AI_Message.Kind]
      ): Option[JSON.Object.Entry] =
        summary_entry(key, count)(JSON.optional(
          without.fold("items")(kind => "without_" + PIDE_AI_Message.Messages.key(kind)) ->
            proper_list(items.getOrElse(key, Nil))))

      val returned = if (show.details.isEmpty) Nil
        else {
          def is_problematic(state: State): Boolean =
            reported_statuses(state).exists(PIDE_AI_Command_Status.problematic) ||
            state.bad.nonEmpty ||
            reported_messages(state).exists(msg => PIDE_AI_Message.Kind.problematic(msg.kind))
          var problematic = 0
          var unproblematic = 0
          states.filter { state =>
            if (is_problematic(state)) {
              problematic += 1
              show.problematic_limit.forall(problematic <= _)
            }
            else {
              unproblematic += 1
              show.unproblematic_limit.forall(unproblematic <= _)
            }
          }
        }

      JSON_Object.flatten(
        Some("total_timing_ms" -> states.map(_.timing_ms).sum) ::
        summary_entry(cmds_key(PIDE_AI_Command_Status.running.name), running.length)(
          running_json(running, doc, show.snippet_lines, long_running_threshold)) ::
        Option.when(show.terminated_slow) {
          val now = Date.now()
          val terminated_slow = states.filter(state =>
            is_terminated_slow(state.command_status, long_running_threshold, now))
          summary_entry(cmds_key("terminated_slow"), terminated_slow.length)(
            terminated_slow_json(terminated_slow, doc, show.snippet_lines, long_running_threshold))
        }.flatten ::
        PIDE_AI_Command_Status.values.toList.filter(PIDE_AI_Command_Status.problematic)
          .map(status => items_entry(cmds_key(status.name), status_counts(status),
            status.message_kind)) :::
        items_entry(Markup.BAD, states.map(_.bad.length).sum, None) ::
        show.message_kinds.map(kind =>
          items_entry(PIDE_AI_Message.Messages.key(kind), message_counts(kind), None)) :::
        List(PIDE_AI_Command_Status.unprocessed, PIDE_AI_Command_Status.finished).map(status =>
          Option.when(status_counts(status) > 0)(
            cmds_key(status.name) -> status_counts(status))) :::
        List(
          Option.when(terminated < states.length)(cmds_key("terminated") -> terminated),
          Some("cmds" -> (JSON_Object("count" -> states.length) ++
            JSON_Object.if_proper(show.details.nonEmpty, JSON_Object(
              "count_returned" -> returned.length,
              "cmds" -> returned.map(_.json(doc, show.details, unicode_symbols, cwd))))))))
    }
  }

  def nodes_states_json(
    resources: PIDE_AI_Resources,
    selections: List[(Document.Snapshot, Selection)],
    pred: Document_Status.Command_Status => Boolean,
    cwd: Option[Path]
  )(json: (Document.Snapshot, List[State]) => JSON.Object.T): JSON.Object.T =
    JSON_Object.flatten(selections.map { (snapshot, selection) =>
      val states = States(snapshot, selection.filter((cmd, _) =>
        pred(PIDE_AI_Command_Status.command_status(snapshot, cmd)))).toList
      States.counted_summary(states.length)(json(snapshot, states))
        .map(resources.print_node_name(snapshot.node_name, cwd) -> _)
    })

  def nodes_running_json(
    resources: PIDE_AI_Resources,
    selections: List[(Document.Snapshot, Selection)],
    snippet_lines: Int,
    long_running_threshold: Time,
    cwd: Option[Path]
  ): JSON.Object.T =
    nodes_states_json(resources, selections, _.is_running, cwd)((snapshot, states) =>
      States.running_json(states, Line.Document(snapshot.node.source), snippet_lines,
        long_running_threshold))

  def nodes_terminated_slow_json(
    resources: PIDE_AI_Resources,
    selections: List[(Document.Snapshot, Selection)],
    snippet_lines: Int,
    long_running_threshold: Time,
    cwd: Option[Path]
  ): JSON.Object.T = {
    val now = Date.now()
    nodes_states_json(resources, selections,
      is_terminated_slow(_, long_running_threshold, now), cwd)((snapshot, states) =>
        States.terminated_slow_json(states, Line.Document(snapshot.node.source), snippet_lines,
          long_running_threshold))
  }

  val bad_lines: PIDE_AI_Node_Cache.Data[List[String]] =
    PIDE_AI_Node_Cache.Data { snapshot =>
      val doc = Line.Document(snapshot.node.source)
      PIDE_AI_Markup.bad(snapshot, Text.Range.full).map(info =>
        PIDE_AI_Util.line_range.tupled(PIDE_AI_Util.range_lines(doc, info.range))).distinct
    }

  def nodes_bad_json(
    resources: PIDE_AI_Resources,
    view: PIDE_AI_Node_Cache.View,
    names: List[Document.Node.Name],
    cwd: Option[Path]
  ): JSON.Object.T =
    JSON_Object(names.flatMap(name => proper_list(view(bad_lines, name))
      .map(resources.print_node_name(name, cwd) -> _)))

  def await(
    session: PIDE_AI_Session,
    commands: List[Command],
    stop: (Document.Snapshot, List[Document_Status.Command_Status]) => Boolean,
    timeout: Option[Time],
    delay: Time,
    check_factor: Double,
    message: Document.Snapshot => JSON.T,
    progress_delay: Time,
    progress: PIDE_AI_Progress
  ): (Document.Snapshot, List[Command], List[Command], Boolean) = {
    val start = Time.now()
    var snapshot = session.snapshot()
    var version = Document_ID.none
    var survivors: List[Command] = Nil
    var removed: List[Command] = Nil
    var pending: List[Command] = Nil
    var terminated: List[Document_Status.Command_Status] = Nil
    var wait = delay
    session.await_while_ready(progress, _ => message(snapshot), wait, progress_delay) {
      val check_start = Time.now()
      snapshot = session.snapshot()
      if (snapshot.version.id != version) {
        version = snapshot.version.id
        val (survivors1, removed1) = commands.partition(snapshot_contains(snapshot, _))
        survivors = survivors1
        removed = removed1
        pending = survivors
        terminated = Nil
      }
      val (now_terminated, still_pending) =
        pending.map(cmd => cmd -> PIDE_AI_Command_Status.command_status(snapshot, cmd))
          .partition(_._2.is_terminated)
      pending = still_pending.map(_._1)
      terminated = now_terminated.map(_._2) ::: terminated
      val stopped = stop(snapshot, still_pending.map(_._2) ::: terminated)
      val now = Time.now()
      wait = delay max (now - check_start).scale(check_factor)
      timeout.foreach(t => wait = wait min (start + t - now))
      Option.when(stopped || timeout.exists(now - start >= _))(
        (snapshot, survivors, removed, !stopped))
    }
  }

  def await_states(
    session: PIDE_AI_Session,
    snapshot: Document.Snapshot,
    selection: Selection,
    stop: (Document.Snapshot, List[Document_Status.Command_Status]) => Boolean,
    timeout: Option[Time],
    delay: Time,
    check_factor: Double,
    message: (Document.Snapshot, Iterator[State]) => JSON.T,
    progress_delay: Time,
    progress: PIDE_AI_Progress
  ): (Document.Snapshot, Iterator[State], Int, Boolean) = {
    def node_states(snapshot1: Document.Snapshot): (Document.Snapshot, Iterator[State]) = {
      val node_snapshot = session.switch(snapshot1, snapshot.node_name)
      (node_snapshot, States(node_snapshot, selection))
    }
    val commands = selection.parts.map(_._1).filterNot(is_ignorable_for_good(snapshot, _))
    val (snapshot1, _, removed, timed_out) = await(session, commands, stop, timeout, delay,
      check_factor, snapshot1 => message.tupled(node_states(snapshot1)), progress_delay, progress)
    val (node_snapshot, states) = node_states(snapshot1)
    (node_snapshot, states, removed.count(!is_ignorable(snapshot, _)), timed_out)
  }

}
