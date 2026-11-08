/*  Title:      AI/src/PIDE/pide_ai_document_command.scala
    Author:     Kevin Kappelmann

Command utilities.
*/

package isabelle.pide.ai

import isabelle._

import scala.collection.mutable

object PIDE_AI_Document_Command {
  val bad = Markup.BAD

  enum Status(
    val name: String,
    val message_kind: Option[PIDE_AI_Message.Message_Kind] = None
  ) {
    // running confuses agents: "running" = "it worked"
    case running extends Status("processing")
    case failed extends Status("failed", Some(PIDE_AI_Message.Message_Kind.errors))
    case warned extends Status("warned", Some(PIDE_AI_Message.Message_Kind.warnings))
    case canceled extends Status("canceled")
    case unprocessed extends Status("unprocessed")
    case finished extends Status("finished")
  }
  object Status {
    val unproblematic: Set[Status] = Set(unprocessed, finished)
    val problematic: Set[Status] = values.toSet -- unproblematic

    val prefix = "cmds_"
    def key(name: String): String = prefix + name
    def prefix_keys(obj: JSON.Object.T): JSON.Object.T =
      JSON_Object.prefix_keys(prefix, values.map(_.name).toSet + bad)(obj)
  }

  def command_status(snapshot: Document.Snapshot, cmd: Command): Document_Status.Command_Status =
    snapshot.state.command_status(snapshot.version, cmd)

  def statuses(status: Document_Status.Command_Status): List[Status] =
    List(
      Option.when(status.is_unprocessed)(Status.unprocessed),
      Option.when(status.is_running)(Status.running),
      Option.when(status.is_failed)(Status.failed),
      Option.when(status.is_warned)(Status.warned),
      Option.when(status.is_finished)(Status.finished),
      Option.when(status.is_canceled)(Status.canceled)).flatten

  def is_ignorable(snapshot: Document.Snapshot, cmd: Command): Boolean =
    cmd.is_ignored &&
      !statuses(command_status(snapshot, cmd)).exists(status =>
        status != Status.running && Status.problematic(status)) &&
      snapshot.command_results(cmd).is_empty
  def is_ignorable_for_good(snapshot: Document.Snapshot, cmd: Command): Boolean =
    is_ignorable(snapshot, cmd) && command_status(snapshot, cmd).is_terminated

  def snapshot_contains(snapshot: Document.Snapshot, cmd: Command): Boolean =
    snapshot.command_node(cmd).commands.contains(cmd)

  // cf. Rendering.overview_color
  def range_status(
    snapshot: Document.Snapshot,
    range: Text.Range
  ): Document_Status.Command_Status = {
    val markups = snapshot.cumulate[List[Markup]](range, Nil,
      Document_Status.Command_Status.liberal_elements,
      _ => { case (status, Text.Info(_, elem)) => Some(elem.markup :: status) }, status = true)
    Document_Status.Command_Status.make(Date.now(), markups = markups.flatMap(_.info))
  }

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
      if Symbol.decode(text) == here => if (self_id(snapshot.state, cmd)(id)) Nil
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

    def filter_status(snapshot: Document.Snapshot, status: Status): Selection =
      filter((cmd, _) => statuses(command_status(snapshot, cmd)).contains(status))
  }

  object Selection {
    def apply(snapshot: Document.Snapshot, range: Option[Text.Range]): Selection =
      PIDE_AI_Nodes.node_command(snapshot.version.nodes, snapshot.node_name) match {
        case Some(cmd) =>
          node_command(cmd, PIDE_AI_Util.restrict_text_range(snapshot.node.source, range))
        case None =>
          commands(
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
  }

  enum Problem {
    case status(status: Status, range: Text.Range, timing_ms: Long)
    case bad(info: Text.Info[String])
    case message(msg: PIDE_AI_Message.Message)

    def key: String =
      this match {
        case Problem.status(status, _, _) => status.name
        case Problem.bad(_) => PIDE_AI_Document_Command.bad
        case Problem.message(msg) => msg.kind.toString
      }

    def message_kind: Option[PIDE_AI_Message.Message_Kind] =
      this match {
        case Problem.status(status, _, _) => status.message_kind
        case Problem.bad(_) => None
        case Problem.message(msg) => Some(msg.kind)
      }

    def ranges: List[Text.Range] =
      this match {
        case Problem.status(_, range, _) => List(range)
        case Problem.bad(info) => List(info.range)
        case Problem.message(msg) => msg.ranges
      }

    def text(cwd: Option[Path]): Option[String] =
      this match {
        case Problem.status(_, _, _) => None
        case Problem.bad(info) => proper_string(info.info)
        case Problem.message(msg) => proper_string(msg.text(cwd))
      }

    def json(doc: Line.Document, snippet_lines: Int, cwd: Option[Path]): JSON.Object.T =
      JSON_Object("snippets" -> ranges.map(PIDE_AI_Util.snippet(doc, _, snippet_lines))) ++
      JSON.optional("message" -> text(cwd)) ++
      JSON.optional("timing_ms" -> Some(this).collect {
        case Problem.status(Status.running, _, timing_ms) => timing_ms
      })
  }

  def node_problems_json(
    resources: Resources,
    node_problems: List[(Document.Snapshot, List[Problem])],
    snippet_lines: Int,
    cwd: Option[Path]
  ): JSON.Object.T =
    JSON_Object.flatten(node_problems.map { (snapshot, problems) =>
      Option.when(problems.nonEmpty) {
        val doc = Line.Document(snapshot.node.source)
        PIDE_AI_Origin.print(resources, snapshot.node_name, cwd) ->
          problems.map(_.json(doc, snippet_lines, cwd))
      }
    })

  object State {
    enum Detail { case errors, warnings, infos, goals, src, types, facts, markup }

    object Detail {
      def of(kind: PIDE_AI_Message.Message_Kind): Detail =
        kind match {
          case PIDE_AI_Message.Message_Kind.goals => goals
          case PIDE_AI_Message.Message_Kind.errors => errors
          case PIDE_AI_Message.Message_Kind.warnings => warnings
          case PIDE_AI_Message.Message_Kind.writeln | PIDE_AI_Message.Message_Kind.information |
            PIDE_AI_Message.Message_Kind.tracing => infos
        }
      require(
        PIDE_AI_Message.Message_Kind.values.forall(kind =>
          PIDE_AI_Message.Message_Kind.info.contains(kind) == (of(kind) == infos)),
        "Inconsistent info message kinds")
    }
  }

  sealed case class State private[PIDE_AI_Document_Command](
    snapshot: Document.Snapshot,
    cmd: Command,
    range: Text.Range,
    messages: List[PIDE_AI_Message.Message],
    unicode_symbols: Boolean
  ) {
    lazy val command_status: Document_Status.Command_Status =
      PIDE_AI_Document_Command.command_status(snapshot, cmd)
    private lazy val range_status = PIDE_AI_Document_Command.range_status(snapshot, range)
    lazy val status: List[Status] = statuses(command_status).filter {
      case Status.warned => range_status.is_warned
      case Status.failed => range_status.is_failed
      case _ => true
    }
    lazy val timing_ms: Long = command_status.timings.sum(Date.now()).ms
    private lazy val messages_by_kind = messages.groupBy(_.kind)
    def messages_of(kind: PIDE_AI_Message.Message_Kind): List[PIDE_AI_Message.Message] =
      messages_by_kind.getOrElse(kind, Nil)
    lazy val errors = messages_of(PIDE_AI_Message.Message_Kind.errors)
    lazy val warnings = messages_of(PIDE_AI_Message.Message_Kind.warnings)
    lazy val bad: List[Text.Info[String]] = PIDE_AI_Markup.bad(snapshot, range, unicode_symbols)
      .filterNot(info => errors.nonEmpty && info.info.isEmpty)
    private def bad_json(doc: Line.Document): List[JSON.Object.T] =
      bad.map { case Text.Info(r, text) =>
        JSON_Object.flatten(
          proper_string(text).map("message" -> _),
          Some("src" ->
            PIDE_AI_Util.numbered_lines(r.substring(snapshot.node.source),
              doc.position(r.start).line1)))
      }

    lazy val problems: List[Problem] =
      status.filter(Status.problematic).map(Problem.status(_, range, timing_ms)) :::
      bad.map(Problem.bad(_)) ::: (errors ::: warnings).map(Problem.message(_))

    def is_ignorable: Boolean = PIDE_AI_Document_Command.is_ignorable(snapshot, cmd)

    def json(doc: Line.Document, details: Set[State.Detail], cwd: Option[Path]): JSON.Object.T = {
      val (start_line, end_line) = PIDE_AI_Util.range_lines(doc, range)
      val entries: List[Option[JSON.Object.Entry]] = List(
        proper_list(status.map(_.name)).map("status" -> _),
        Option.when(timing_ms > 0)("timing_ms" -> timing_ms),
        Some(
          if (details(State.Detail.src))
            "src" -> PIDE_AI_Util.numbered_lines(
              range.substring(snapshot.node.source).stripLineEnd, start_line)
          else "lines" -> PIDE_AI_Util.line_range(start_line, end_line)),
        proper_list(bad_json(doc)).map(PIDE_AI_Document_Command.bad -> _)) :::
        PIDE_AI_Message.messages_json(messages_by_kind,
          PIDE_AI_Message.Message_Kind.values.toList.filter(kind =>
            details(State.Detail.of(kind))), cwd) :::
        List(
          Option.when(details(State.Detail.types))(
            proper_list(PIDE_AI_Markup.types_json(snapshot, range, unicode_symbols))
              .map("types" -> _)).flatten,
          Option.when(details(State.Detail.facts))(
            proper_list(PIDE_AI_Markup.facts_json(snapshot, range, unicode_symbols))
              .map("facts" -> _)).flatten,
          Option.when(details(State.Detail.markup))(
            "markup" -> PIDE_AI_Markup.markup_json(snapshot, range, Markup.Elements.full,
              unicode_symbols)))
      JSON_Object.flatten(entries)
    }
  }

  object States {
    def command_spans(
      snapshot: Document.Snapshot,
      theory_cmd: Command,
      range: Option[Text.Range],
      unicode_symbols: Boolean
    ): Iterator[State] =
      snapshot.command_spans(PIDE_AI_Util.restrict_text_range(snapshot.node.source, range))
        .iterator.map { span =>
          val restricted = PIDE_AI_Util.intersect_range(span.range, range)
          State(snapshot, theory_cmd, restricted,
            Rendering.text_messages(snapshot, restricted).flatMap(info =>
              PIDE_AI_Message.Message_Kind.of(info.info).map(kind =>
                PIDE_AI_Message.Message(kind, info.info, List(info.range), snapshot,
                  Some(theory_cmd), unicode_symbols))),
            unicode_symbols)
        }

    def apply(
      snapshot: Document.Snapshot,
      selection: Selection,
      unicode_symbols: Boolean
    ): Iterator[State] =
      selection match {
        case Selection.node_command(cmd, range) =>
          if (snapshot.node_name.is_theory)
            command_spans(snapshot, cmd, Some(range), unicode_symbols)
          else {
            val messages =
              PIDE_AI_Message.messages(snapshot, cmd, 0, range, unicode_symbols)
            Iterator(State(snapshot, cmd, range, messages, unicode_symbols))
          }
        case Selection.commands(cmds) =>
          val starts = snapshot.node.command_iterator().toMap
          for ((cmd, part) <- cmds.iterator; start <- starts.get(cmd)) yield {
            val range = part + start
            State(snapshot, cmd, range,
              PIDE_AI_Message.messages(snapshot, cmd, start, range, unicode_symbols),
              unicode_symbols)
          }
      }

    def apply(
      snapshot: Document.Snapshot,
      range: Option[Text.Range],
      unicode_symbols: Boolean
    ): Iterator[State] =
      apply(snapshot, Selection(snapshot, range), unicode_symbols)

    sealed case class Report(
      details: Set[State.Detail],
      summary_message_kinds: List[PIDE_AI_Message.Message_Kind],
      problematic_limit: Option[Int],
      unproblematic_limit: Option[Int],
      occurrences_limit: Option[Int],
      snippet_lines: Int)

    def json(
      snapshot: Document.Snapshot,
      states: Iterator[State],
      report: Report,
      cwd: Option[Path]
    ): JSON.Object.T = {
      val doc = Line.Document(snapshot.node.source)
      def redundant_problem(state: State, problem: Problem): Boolean =
        problem.text(cwd).isEmpty && problem.message_kind.exists(state.messages_of(_).nonEmpty)
      val counts = mutable.Map[String, Int]().withDefaultValue(0)
      val occurrences = mutable.Map[String, mutable.ListBuffer[JSON.Object.T]]()
      val returned_commands = new mutable.ListBuffer[JSON.Object.T]
      var total_timing_ms = 0L
      var count = 0
      var count_problematic = 0
      var count_unproblematic = 0
      var count_listed_occurrences = 0
      val include_details = report.details.nonEmpty
      for (state <- states if !state.is_ignorable) {
        count += 1
        total_timing_ms += state.timing_ms
        for (status <- state.status if Status.unproblematic(status)) counts(status.name) += 1
        val problems = state.problems.filter(
          _.message_kind.forall(report.summary_message_kinds.contains))
        for (problem <- problems) {
          counts(problem.key) += 1
          if (report.occurrences_limit.forall(count_listed_occurrences < _) &&
              !redundant_problem(state, problem)) {
            occurrences.getOrElseUpdate(problem.key, new mutable.ListBuffer[JSON.Object.T]) +=
              problem.json(doc, report.snippet_lines, cwd)
            count_listed_occurrences += 1
          }
        }
        if (include_details) {
          if (problems.nonEmpty) {
            if (report.problematic_limit.forall(count_problematic < _))
              returned_commands += state.json(doc, report.details, cwd)
            count_problematic += 1
          }
          else {
            if (report.unproblematic_limit.forall(count_unproblematic < _))
              returned_commands += state.json(doc, report.details, cwd)
            count_unproblematic += 1
          }
        }
      }
      def occurrences_entry(
        key: String,
        message_kind: Option[PIDE_AI_Message.Message_Kind]
      ): Option[JSON.Object.Entry] =
        Option.when(counts(key) > 0) {
          val name = message_kind.fold("occurrences")("without_" + _)
          key -> (JSON_Object("count" -> counts(key)) ++
            JSON.optional(name -> occurrences.get(key).map(_.toList)))
        }
      def count_entry(key: String): Option[JSON.Object.Entry] =
        Option.when(counts(key) > 0)(key -> counts(key))

      Status.prefix_keys(JSON_Object.flatten(
        Some("total_timing_ms" -> total_timing_ms) ::
        Status.values.toList.filter(Status.problematic)
          .map(status => occurrences_entry(status.name, status.message_kind)) :::
        List(bad, PIDE_AI_Message.Message_Kind.errors.toString,
          PIDE_AI_Message.Message_Kind.warnings.toString)
          .map(occurrences_entry(_, None)) :::
        Status.values.toList.filter(Status.unproblematic)
          .map(status => count_entry(status.name)))) +
      ("cmds" -> (JSON_Object("count" -> count) ++
        JSON_Object.if_proper(include_details, JSON_Object(
          "count_returned" -> returned_commands.length, "cmds" -> returned_commands.toList))))
    }
  }

  def await(
    session: PIDE_AI_Session,
    commands: List[Command],
    stop: List[Document_Status.Command_Status] => Boolean,
    timeout: Option[Time],
    delay: Time,
    check_factor: Double,
    progress: Progress
  ): (Document.Snapshot, List[Command], List[Command], Boolean) = {
    val start = Time.now()
    var snapshot = session.snapshot()
    def count(cmds: List[Command]): Int = cmds.count(!is_ignorable(snapshot, _))
    var version = Document_ID.none
    var survivors: List[Command] = Nil
    var removed: List[Command] = Nil
    var pending: List[Command] = Nil
    var terminated: List[Document_Status.Command_Status] = Nil
    var wait = delay
    session.await_while_ready(progress,
      s"commands (${count(survivors) - count(pending)}/${count(survivors)} terminated)", wait) {
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
        pending.map(cmd => cmd -> command_status(snapshot, cmd))
          .partition(_._2.is_terminated)
      pending = still_pending.map(_._1)
      terminated = now_terminated.map(_._2) ::: terminated
      val stopped = stop(still_pending.map(_._2) ::: terminated)
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
    stop: List[Document_Status.Command_Status] => Boolean,
    timeout: Option[Time],
    delay: Time,
    check_factor: Double,
    progress: Progress
  ): (Document.Snapshot, Iterator[State], Int, Boolean) = {
    val commands = selection.parts.map(_._1).filterNot(is_ignorable_for_good(snapshot, _))
    val (snapshot1, _, removed, timed_out) =
      await(session, commands, stop, timeout, delay, check_factor, progress)
    val node_snapshot = session.switch(snapshot1, snapshot.node_name)
    (node_snapshot, States(node_snapshot, selection, session.unicode_symbols),
      removed.count(!is_ignorable(snapshot, _)), timed_out)
  }

  def status_problems(
    selections: List[(Document.Snapshot, Selection)],
    status: Status,
    unicode_symbols: Boolean
  ): List[(Document.Snapshot, List[Problem])] =
    for ((snapshot, selection) <- selections)
    yield snapshot -> States(snapshot, selection.filter_status(snapshot, status), unicode_symbols)
      .flatMap(_.problems.filter(_.key == status.name)).toList
}
