/*  Title:      MCP/src/task.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.mcp

import isabelle._

object Task {
  def fork[A](name: String, task: Task[A], daemon: Boolean = false): Future[A] = {
    val result = Future.promise[A]
    Isabelle_Thread.fork(name = name, daemon = daemon) {
      result.fulfill_result(Exn.capture { task.run() })
    }
    new Future[A] {
      def peek: Option[Exn.Result[A]] = result.peek
      def join_result: Exn.Result[A] = result.join_result
      def cancel(): Unit = task.stop()
    }
  }

  def close_on_stop[A](resource: AutoCloseable, what: String, log: Logger)(body: => A): Task[A] =
    new Task[A] {
      def run(): A = body
      def stop(): Unit =
        PIDE_MCP_Util.log_failure(log, "Failed to close " + what,
          Exn.capture { resource.close() })
    }
}

trait Task[A] {
  def run(): A
  def stop(): Unit
}
