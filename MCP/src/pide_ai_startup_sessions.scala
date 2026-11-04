/*  Title:      MCP/src/pide_ai_startup_sessions.scala
    Author:     Kevin Kappelmann

Sessions requested at startup.
*/

package isabelle.pide.mcp

import isabelle._

object PIDE_AI_Startup_Sessions {
  def await(options: Options): Boolean = options.bool("pide_mcp_await_option_sessions")
  def exit_on_failure(options: Options): Boolean =
    options.bool("pide_mcp_exit_on_failed_option_sessions")
}

class PIDE_AI_Startup_Sessions(
  specs: List[PIDE_MCP_Session_Spec],
  await: Boolean,
  exit_on_failure: Boolean,
  log: Logger,
  base_progress: Progress.Status,
  start_session: (PIDE_MCP_Session_Spec, Progress) => Unit
) extends Progress_Task[Unit](new Sub_Progress(base_progress) with Progress.Local_Interrupts) {
  private val finished = Future.promise[Unit]
  val ready: Future[Unit] = if (await) finished else Future.value(())

  def run(): Unit = {
    val result = Exn.capture {
      if (specs.nonEmpty) log("Starting PIDE sessions...")
      for (spec <- specs) {
        progress.expose_interrupt()
        Exn.capture { start_session(spec, progress) } match {
          case Exn.Res(_) =>
          case Exn.Exn(exn) =>
            progress.expose_interrupt()
            if (exit_on_failure || Exn.is_interrupt(exn)) throw exn
            else log.error_message(s"Failed to start PIDE session: ${Exn.message(exn)}")
        }
      }
    }
    finished.fulfill_result(result)
    Exn.release(result)
  }
}
