/*  Title:      AI/src/General/result.scala
    Author:     Kevin Kappelmann

Results or expected errors
*/

package isabelle.pide.ai

import isabelle._

enum Result[+A, +E] {
  case Res(value: A) extends Result[A, Nothing]
  case Error(error: E) extends Result[Nothing, E]

  def map_res[B](f: A => B): Result[B, E] =
    this match {
      case Res(value) => Res(f(value))
      case Error(error) => Error(error)
    }
}
