/*  Title:      AI/src/Server/pide_ai_tool_call.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.ai

import isabelle._

class PIDE_AI_Tool_Call(
  server: PIDE_AI_Server,
  tool: PIDE_AI_Tool,
  args: JSON.Object.T,
  progress: Progress,
  log: Logger,
  context: PIDE_AI_Client_Context
) extends Progress_Task[PIDE_AI_Tool_Result](progress) {
  def run(): PIDE_AI_Tool_Result =
    Exn.capture {
      progress.expose_interrupt()
      PIDE_AI_Tool_Result.result(tool.check_args(args)) match {
        case Result.Error(error) => Result.Error(error)
        case Result.Res(_) => tool.handle(server, args, progress, context)
      }
    } match {
      case Exn.Res(result) => result
      case Exn.Exn(Exn.Interrupt()) if progress.stopped => Result.Error("Tool call was stopped")
      case Exn.Exn(exn) => Result.Error(
        PIDE_AI_Util.handle_internal_error(log, "tool " + quote(tool.id), exn))
    }
}
