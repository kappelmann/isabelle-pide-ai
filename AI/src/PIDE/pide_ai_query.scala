/*  Title:      AI/src/PIDE/pide_ai_query.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.ai

import isabelle._

object PIDE_AI_Query {
  enum Outcome { case finished, stopped, timeout }
  sealed case class Result(
    outcome: Outcome,
    query_time: Time,
    output: Editor.Output,
    command: Option[Command]
  ) {
    def messages: List[PIDE_AI_Message] = {
      val ranges = command.flatMap(cmd =>
        output.snapshot.command_start(cmd).map(cmd.range + _)).toList
      output.messages.flatMap(elem =>
        PIDE_AI_Message.Kind.of(elem).map(kind =>
          PIDE_AI_Message(kind, elem, ranges, output.snapshot, command)))
    }

    def json(
      kinds: List[PIDE_AI_Message.Kind],
      unicode_symbols: Boolean,
      cwd: Option[Path]
    ): JSON.Object.T =
      JSON_Object.flatten(
        List(Some("status" -> outcome.toString), Some("query_ms" -> query_time.ms)) :::
        PIDE_AI_Message.Messages.json(messages.groupBy(_.kind), kinds, unicode_symbols, cwd))
  }
}

class PIDE_AI_Query(
  session: PIDE_AI_Session,
  context: PIDE_AI_Editor.Context,
  operation: String,
  progress: PIDE_AI_Progress,
  cwd: Option[Path]
) {
  private val editor = session.editor(progress)
  private val status = Synchronized(Query_Operation.Status.waiting)
  private val output = Synchronized(Editor.Output.init)
  private val query: Query_Operation = new Query_Operation(editor, context, operation,
    st => { status.change(_ => st); progress.echo(s"$operation $st") },
    out =>
      for {
        msg <- output.change_result(old => (out.messages.diff(old.messages), out))
        kind <- PIDE_AI_Message.Kind.of(msg)
      } {
        val text = PIDE_AI_Message(kind, msg, Nil, out.snapshot, query.get_location)
          .text(session.resources.unicode_symbols, cwd)
        kind match {
          case PIDE_AI_Message.Kind.error => progress.echo_error_message(text)
          case PIDE_AI_Message.Kind.warning => progress.echo_warning(text)
          case _ => progress.echo(text)
        }
      })

  private def node: String = quote(session.resources.print_node_name(context.node_name, cwd))

  def finished: Boolean = status.value == Query_Operation.Status.finished

  def require_snapshot(): Document.Snapshot =
    editor.current_node_snapshot(context).getOrElse(
      error(s"No PIDE snapshot available for $node"))

  private def require_command(): Unit =
    for (command <- query.get_location) {
      val snapshot = require_snapshot()
      if (!PIDE_AI_Document_Command.snapshot_contains(snapshot, command))
        error(s"The command of the $operation query in $node is gone")
      val print_function = quote(operation + "_query")
      for {
        msg <- snapshot.command_results(command).iterator.map(_._2)
          .filter(Protocol.is_error).map(XML.content).find(_.contains(print_function))
      } error("Query error: " + msg)
    }

  def run(
    args: List[String],
    timeout: Option[Time] = None,
    stop: Editor.Output => Boolean = _ => false
  ): PIDE_AI_Query.Result = {
    val snapshot = require_snapshot()
    if (!session.resources.is_loaded_dynamic_node(snapshot.version.nodes, context.node_name))
      error(s"Cannot query $node: not a dynamic node")
    if (editor.current_command(context, snapshot).isEmpty)
      error(s"No command at the given position of $node")

    val start = Time.now()
    query.activate()
    try {
      editor.send_wait_dispatcher { query.apply_query(args) }
      val outcome = session.await_while_ready(progress,
        elapsed => "Awaiting " + operation + " " + PIDE_AI_Progress.elapsed_text(elapsed),
        session.poll_delay, session.progress_delay) {
        if (finished) Some(PIDE_AI_Query.Outcome.finished)
        else if (stop(output.value)) Some(PIDE_AI_Query.Outcome.stopped)
        else {
          require_command()
          Option.when(timeout.exists(Time.now() - start > _))(PIDE_AI_Query.Outcome.timeout)
        }
      }
      PIDE_AI_Query.Result(outcome, Time.now() - start, output.value, query.get_location)
    }
    finally {
      if (!finished) Exn.capture { editor.send_wait_dispatcher { query.cancel_query() } }
      query.deactivate()
    }
  }
}
