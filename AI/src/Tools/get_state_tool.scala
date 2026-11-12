/*  Title:      AI/src/Tools/get_state_tool.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.ai

import isabelle._

object Get_State_Tool {
  def delay(options: Options): Time =
    options.seconds("pide_ai_get_state_delay")
  def wait_ratio(options: Options): Double =
    options.real("pide_ai_get_state_wait_ratio")
  val progress_delay_option: String = "pide_ai_get_state_progress_delay"
  def progress_delay(options: Options): Time = options.seconds(progress_delay_option)

  enum Summary { case warnings, terminated_slow }

  val other_cmds_running_key: String =
    "other_" + PIDE_AI_Document_Command.States.cmds_key(PIDE_AI_Command_Status.running.name)

  enum Show_Other_Cmds_Running {
    case never, if_range_unterminated, always
    def show(unterminated: Boolean): Boolean =
      this == always || this == if_range_unterminated && unterminated
  }

  def base_description(which_commands: String): String =
    "Summary (errors, commands being processed,...) and optional details " +
    s"(subgoals, types, markup,...) of $which_commands. Limits do not affect counts. " +
    "Commands edited during request are omitted. " +
    "For a non-theory file, the state is that of the command loading it."

  object Request {
    val summary_arg = JSON_Schema.Arg.default("summary",
      "Optional entries. " +
        s"${Summary.terminated_slow} = above ${PIDE_AI_Tool_Schema.long_running_arg.name}.",
      JSON_Schema.Format.list(JSON_Schema.Format.enumeration(Summary.values)),
      List(Summary.terminated_slow))
    val summary_problematic_limit_arg = JSON_Schema.Arg.optional(
      "summary_problematic_limit",
      "Problematic items to return. Default: all.",
      JSON_Schema.Format.int(minimum = Some(0)))
    val snippet_lines_arg = JSON_Schema.Arg.default(
      "snippet_lines", "Number of source lines shown (0 for line ranges only).",
      JSON_Schema.Format.int(minimum = Some(0)), 1)
    val details_arg = JSON_Schema.Arg.default("details",
      "Details to return. infos = " + commas(PIDE_AI_Document_Command.State.Detail.kinds(
        PIDE_AI_Document_Command.State.Detail.infos).map(PIDE_AI_Message.Messages.key)) + ".",
      JSON_Schema.Format.list(
        JSON_Schema.Format.enumeration(PIDE_AI_Document_Command.State.Detail.values)),
      Nil)
    val details_problematic_limit_arg = JSON_Schema.Arg.optional(
      "details_problematic_limit",
      "Problematic commands to return. Default: all.",
      JSON_Schema.Format.int(minimum = Some(0)))
    val details_unproblematic_limit_arg = JSON_Schema.Arg.optional(
      "details_unproblematic_limit",
      "Unproblematic commands to return. Default: all.",
      JSON_Schema.Format.int(minimum = Some(0)))
    val show_thy_arg = JSON_Schema.Arg.default("show_thy", "Status or nothing.",
      JSON_Schema.Format.enumeration(PIDE_AI_Theory_Status.Statuses.Show.values
        .filterNot(_ == PIDE_AI_Theory_Status.Statuses.Show.names)),
      PIDE_AI_Theory_Status.Statuses.Show.not_ok)
    val show_ancestor_thys_arg = PIDE_AI_Tool_Schema.show_thy_statuses_arg(
      "show_ancestor_thys", PIDE_AI_Theory_Status.Statuses.Show.not_ok)
    val show_descendant_thys_arg = PIDE_AI_Tool_Schema.show_thy_statuses_arg(
      "show_descendant_thys", PIDE_AI_Theory_Status.Statuses.Show.none)
    val show_other_cmds_running_arg = JSON_Schema.Arg.default(
      "show_" + other_cmds_running_key, "",
      JSON_Schema.Format.enumeration(Show_Other_Cmds_Running.values),
      Show_Other_Cmds_Running.if_range_unterminated)

    def timeout_arg(default: Time, subtracted: String) = JSON_Schema.Arg.default(
      "timeout_ms", s"Max. wait. $subtracted time gets subtracted.",
      PIDE_AI_Tool_Schema.ms_format(0), default)
    val progress_delay_arg = PIDE_AI_Tool_Schema.progress_delay_arg(progress_delay_option)

    def args(timeout_arg: JSON_Schema.Arg[Time]): List[JSON_Schema.Arg[?]] =
      List(summary_arg, PIDE_AI_Tool_Schema.long_running_arg, summary_problematic_limit_arg,
        snippet_lines_arg, details_arg, details_problematic_limit_arg,
        details_unproblematic_limit_arg, show_thy_arg, show_ancestor_thys_arg,
        show_descendant_thys_arg, show_other_cmds_running_arg, timeout_arg,
        PIDE_AI_Tool_Schema.stop_on_arg, progress_delay_arg,
        PIDE_AI_Tool_Schema.disk_sync_arg)

    def from_json(args: JSON.Object.T, timeout_arg: JSON_Schema.Arg[Time]): Request = {
      val summary = summary_arg.get(args).toSet
      val message_kinds = PIDE_AI_Message.Kind.error ::
        Option.when(summary(Summary.warnings))(PIDE_AI_Message.Kind.warning).toList
      Request(
        PIDE_AI_Document_Command.States.Show(message_kinds, summary(Summary.terminated_slow),
          summary_problematic_limit_arg.get(args), snippet_lines_arg.get(args),
          details_arg.get(args).toSet, details_problematic_limit_arg.get(args),
          details_unproblematic_limit_arg.get(args)),
        PIDE_AI_Tool_Schema.long_running_arg.get(args), show_thy_arg.get(args),
        show_ancestor_thys_arg.get(args), show_descendant_thys_arg.get(args),
        show_other_cmds_running_arg.get(args), timeout_arg.get(args),
        PIDE_AI_Tool_Schema.stop_on_arg.get(args), progress_delay_arg.get(args),
        PIDE_AI_Tool_Schema.disk_sync_arg.get(args))
    }
  }

  sealed case class Request(
    show: PIDE_AI_Document_Command.States.Show,
    long_running_threshold: Option[Time],
    show_thy: PIDE_AI_Theory_Status.Statuses.Show,
    show_ancestor_thys: PIDE_AI_Theory_Status.Statuses.Show,
    show_descendant_thys: PIDE_AI_Theory_Status.Statuses.Show,
    show_other_cmds_running: Show_Other_Cmds_Running,
    timeout: Time,
    stop_on: PIDE_AI_Tool_Util.Stop_On,
    progress_delay: Option[Time],
    sync: PIDE_AI_Tool_Util.Sync)

  def state(
    session: PIDE_AI_Session,
    node_name: Document.Node.Name,
    start_line: Int,
    opt_end_line: Option[Int],
    request: Request,
    start: Time,
    progress: PIDE_AI_Progress,
    cwd: Option[Path]
  ): JSON.Object.T = {
    val snapshot0 = PIDE_AI_Tool_Util.require_stable_synced_node_snapshot(
      session, node_name, request.sync, progress, cwd)
    val range = PIDE_AI_Util.check_text_range(
      Line.Document(snapshot0.node.source), start_line, opt_end_line)
    val selection = PIDE_AI_Document_Command.Selection(snapshot0, Some(range))
    val resources = session.resources
    val long_running_threshold = request.long_running_threshold.getOrElse(
      session.long_running_threshold)

    def json(
      snapshot: Document.Snapshot,
      states: List[PIDE_AI_Document_Command.State],
      edited: Int,
      timed_out: Boolean
    ): JSON.Object.T = {
      val states_json = PIDE_AI_Document_Command.States.json(states, request.show,
        Line.Document(snapshot.node.source), long_running_threshold, resources.unicode_symbols, cwd)
      val unterminated = states.exists(!_.command_status.is_terminated)
      lazy val view = session.node_cache.view(session.snapshot())
      lazy val nodes = view.snapshot.version.nodes
      lazy val dynamic_thys = PIDE_AI_Nodes.Loaded(resources, nodes).dynamic
      lazy val thys = PIDE_AI_Nodes.node_theory(nodes, node_name).toList
      val blocked = PIDE_AI_Nodes.blocked_by(resources, snapshot)
      def statuses_json(
        show: PIDE_AI_Theory_Status.Statuses.Show,
        names: => Set[Document.Node.Name]
      ): JSON.Object.T =
        show match {
          case PIDE_AI_Theory_Status.Statuses.Show.none => JSON.Object.empty
          case _ =>
            val statuses = PIDE_AI_Theory_Status.Statuses(view,
              dynamic_thys.filter(names -- thys))
            if (!statuses.exists(status => show.contains(status.overall_status))) JSON.Object.empty
            else PIDE_AI_Theory_Status.Statuses.json(resources, statuses, show, cwd)
        }
      val thy = thys.map(view(PIDE_AI_Theory_Status.data, _))
        .find(status => request.show_thy.contains(status.overall_status))
        .map(_.json(resources.unicode_symbols, cwd))
      val ancestors = statuses_json(request.show_ancestor_thys,
        resources.known_dependency_closure(nodes, thys).toSet)
      val descendants = statuses_json(request.show_descendant_thys,
        resources.known_descendant_closure(nodes, thys).toSet)
      val other_cmds_running =
        if (!request.show_other_cmds_running.show(unterminated)) JSON.Object.empty
        else {
          val selected = selection.parts.map(_._1).toSet
          val selections = PIDE_AI_Document_Command.Selection.running(session, view, dynamic_thys)
            .map((node_snapshot, running) =>
              node_snapshot -> running.filter((cmd, _) => !selected(cmd)))
          PIDE_AI_Document_Command.nodes_running_json(resources, selections, 1,
            long_running_threshold, cwd)
        }
      states_json ++ JSON_Object.flatten(Option.when(edited > 0)(
        PIDE_AI_Document_Command.States.cmds_key("edited_omitted") -> edited),
        Option.when(timed_out)("timed_out" -> true),
        Option.when(blocked.nonEmpty)("blocked_by" -> JSON_Object(blocked.map((name, header) =>
          resources.print_node_name(name, cwd) ->
            header.errors.map(_.text(resources.unicode_symbols, cwd))))),
        thy.map("thy" -> _),
        Option.when(ancestors.nonEmpty)("ancestors" -> ancestors),
        Option.when(descendants.nonEmpty)("descendants" -> descendants),
        Option.when(other_cmds_running.nonEmpty)(
          other_cmds_running_key -> other_cmds_running))
    }

    if (request.timeout.is_zero)
      json(snapshot0, PIDE_AI_Document_Command.States(snapshot0, selection).toList, 0, false)
    else {
      val (snapshot, states, edited, timed_out) = PIDE_AI_Document_Command.await_states(session,
        snapshot0, selection,
        (snapshot, statuses) => request.stop_on.stop(statuses) ||
          PIDE_AI_Nodes.blocked_by(resources, snapshot.switch(node_name)).nonEmpty,
        Some((request.timeout - (Time.now() - start)) max Time.zero),
        delay(session.session_options), wait_ratio(session.session_options),
        (snapshot, states) => json(snapshot, states.toList, 0, false),
        request.progress_delay.getOrElse(progress_delay(session.session_options)),
        progress)
      json(snapshot, states.toList, edited, timed_out)
    }
  }
}

class Get_State_Tool extends PIDE_AI_Tool("get_state") with PIDE_AI_Tool.Read_Only {
  def description: String =
    Get_State_Tool.base_description("commands in range") + " " +
    PIDE_AI_Tool_Schema.implicit_load_files

  val timeout_arg = Get_State_Tool.Request.timeout_arg(Time.zero, "Sync")

  def input_schema: JSON.Object.T = JSON_Schema.object_schema(
    List(PIDE_AI_Tool_Schema.running_session_arg, PIDE_AI_Tool_Schema.node_arg,
      PIDE_AI_Tool_Schema.start_line_default_arg, PIDE_AI_Tool_Schema.opt_end_line_arg) :::
    Get_State_Tool.Request.args(timeout_arg))

  def handle(
    server: PIDE_AI_Server,
    args: JSON.Object.T,
    progress: PIDE_AI_Progress,
    context: PIDE_AI_Client_Context
  ): PIDE_AI_Tool_Result =
    PIDE_AI_Tool_Result.result {
      val start = Time.now()
      val session = PIDE_AI_Tool_Util.running_session_param(server.sessions, args)
      val node_name = PIDE_AI_Tool_Util.node_param(session, args, context.cwd)
      Get_State_Tool.state(session, node_name,
        PIDE_AI_Tool_Schema.start_line_default_arg.get(args),
        PIDE_AI_Tool_Schema.opt_end_line_arg.get(args),
        Get_State_Tool.Request.from_json(args, timeout_arg), start, progress, context.cwd)
    }
}

class Get_State_Tools extends PIDE_AI_Tools(new Get_State_Tool)
