/*  Title:      AI/src/Concurrent/task.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.ai

import isabelle._

object Task {
  trait Forked[A] extends Future[A] {
    def thread: Thread
  }

  def fork[A](name: String, task: Task[A], daemon: Boolean = false): Forked[A] = {
    val result = Future.promise[A]
    val forked_thread = Isabelle_Thread.fork(name = name, daemon = daemon) {
      result.fulfill_result(Exn.capture { task.run() })
    }
    new Forked[A] {
      def thread: Thread = forked_thread
      def peek: Option[Exn.Result[A]] = result.peek
      def join_result: Exn.Result[A] = result.join_result
      def cancel(): Unit = task.stop()
    }
  }

  def close_on_stop[A](resource: AutoCloseable, what: String, log: Logger)(body: => A): Task[A] =
    new Task[A] {
      def run(): A = body
      def stop(): Unit =
        PIDE_AI_Util.log_failure(log, "Failed to close " + what,
          Exn.capture { resource.close() })
    }
}

trait Task[A] {
  def run(): A
  def stop(): Unit
}
