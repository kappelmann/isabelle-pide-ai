/*  Title:      AI/src/Stdio/stdio_connection.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.ai

import isabelle._

class Stdio_Connection(log: Logger) extends Line_Connection(System.in.nn, System.out.nn, log) {
  val remote = "stdin/stdout"
  def shutdown_output(): Unit = ()
}
