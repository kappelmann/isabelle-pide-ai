/*  Title:      MCP/src/progress_task.scala
    Author:     Kevin Kappelmann

Tasks stopped via their progress.
*/

package isabelle.pide.mcp

import isabelle._

abstract class Progress_Task[A](val progress: Progress) extends Task[A] {
  def stop(): Unit = progress.stop()
}
