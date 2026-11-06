/*  Title:      AI/src/PIDE/pide_ai_query.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.ai

import isabelle._

object PIDE_AI_Query {
  enum Outcome { case finished, stopped, timeout }
  sealed case class Result(
    outcome: Outcome,
    elapsed: Time,
    output: Editor.Output,
    command: Option[Command],
    unicode_symbols: Boolean
  ) {
    def messages: List[PIDE_AI_Message.Message] = {
      val ranges = command.flatMap(cmd =>
        output.snapshot.command_start(cmd).map(cmd.range + _)).toList
      output.messages.flatMap(elem =>
        PIDE_AI_Message.Message_Kind.of(elem).map(kind =>
          PIDE_AI_Message.Message(kind, elem, ranges, output.snapshot, command,
            unicode_symbols)))
    }

    def json(kinds: List[PIDE_AI_Message.Message_Kind], cwd: Option[Path]): JSON.Object.T =
      JSON_Object.flatten(
        List(Some("status" -> outcome.toString), Some("elapsed_ms" -> elapsed.ms)) :::
        PIDE_AI_Message.messages_json(messages.groupBy(_.kind), kinds, cwd))
  }
}

class PIDE_AI_Query(
  session: PIDE_AI_Session,
  context: PIDE_AI_Editor.Context,
  operation: String,
  progress: Progress,
  cwd: Option[Path]
) {
  private val editor = session.editor(progress)
  private val status = Synchronized(Query_Operation.Status.waiting)
  private val output = Synchronized(Editor.Output.init)
  private val query = new Query_Operation(editor, context, operation,
    st => { status.change(_ => st); progress.echo(s"$operation $st") },
    out =>
      for (msg <- output.change_result(old => (out.messages.diff(old.messages), out))) {
        val text = PIDE_AI_Util.elem_body_plain_text(msg, session.unicode_symbols)
        if (Protocol.is_error(msg)) progress.echo_error_message(text)
        else if (Protocol.is_warning_or_legacy(msg)) progress.echo_warning(text)
        else progress.echo(text)
      })

  private def origin: String = quote(session.print_origin(context.node_name, cwd))

  def finished: Boolean = status.value == Query_Operation.Status.finished

  def require_snapshot(): Document.Snapshot =
    editor.current_node_snapshot(context).getOrElse(
      error(s"No PIDE snapshot available for $origin"))

  private def require_command(): Unit =
    for (command <- query.get_location)
      if (!require_snapshot().node.commands.contains(command))
        error(s"The command of the $operation query in $origin is gone")

  def run(
    args: List[String],
    timeout: Option[Time] = None,
    stop: Editor.Output => Boolean = _ => false
  ): PIDE_AI_Query.Result = {
    if (session.is_base_session_theory(context.node_name))
      error(s"Cannot query base session theory $origin")
    if (editor.current_command(context, require_snapshot()).isEmpty)
      error(s"No command at the given position of $origin")

    val start = Time.now()
    query.activate()
    try {
      editor.send_wait_dispatcher { query.apply_query(args) }
      val outcome = session.await_while_ready(progress, operation) {
        if (finished) Some(PIDE_AI_Query.Outcome.finished)
        else if (stop(output.value)) Some(PIDE_AI_Query.Outcome.stopped)
        else {
          require_command()
          Option.when(timeout.exists(Time.now() - start > _))(PIDE_AI_Query.Outcome.timeout)
        }
      }
      PIDE_AI_Query.Result(outcome, Time.now() - start, output.value, query.get_location,
        session.unicode_symbols)
    }
    finally {
      if (!finished) Exn.capture { editor.send_wait_dispatcher { query.cancel_query() } }
      query.deactivate()
    }
  }
}
