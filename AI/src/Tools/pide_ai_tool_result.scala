/*  Title:      AI/src/Tools/pide_ai_tool_result.scala
    Author:     Kevin Kappelmann

Results of tool calls.
*/

package isabelle.pide.ai

import isabelle._

type PIDE_AI_Tool_Result = Result[JSON.T, JSON.T]

object PIDE_AI_Tool_Result {
  export Result.{Res, Error}

  def exn_error(exn: Throwable): PIDE_AI_Tool_Result = Result.Error(Exn.message(exn))

  def result(body: => JSON.T): PIDE_AI_Tool_Result =
    Exn.result(body) match {
      case Exn.Res(value) => Result.Res(value)
      case Exn.Exn(exn) => exn_error(exn)
    }}
