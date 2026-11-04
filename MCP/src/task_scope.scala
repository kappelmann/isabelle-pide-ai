/*  Title:      MCP/src/task_scope.scala
    Author:     Kevin Kappelmann

Structured concurrency. An exception escaping a task stops all tasks of its scope.
*/

package isabelle.pide.mcp

import isabelle._

class Task_Scope[T <: Task[?]] {
  private val running = Synchronized[List[T]](Nil)
  def tasks: List[T] = running.value

  private val stop_request = Future.promise[Unit]
  def join_stop_request(): Unit = stop_request.join

  private val failures_while_stopping = Synchronized[List[Throwable]](Nil)

  private def request_stop(result: Exn.Result[Unit]): Unit = {
    val stopping = running.change_result { list =>
      if (stop_request.is_finished) {
        result match {
          case Exn.Exn(exn) if !Exn.is_interrupt(exn) => failures_while_stopping.change(exn :: _)
          case _ =>
        }
        (Nil, list)
      }
      else {
        stop_request.fulfill_result(result)
        (list, list)
      }
    }
    PIDE_MCP_Util.release_combined(stopping.map(task => Exn.capture { task.stop() }))
  }

  def stop(): Unit = request_stop(Exn.Res(()))
  def fail(exn: Throwable): Unit = request_stop(Exn.Exn(exn))

  private def register(task: T): Boolean =
    running.change_result(list => (stop_request.is_finished, task :: list))

  private def unregister(task: T): Unit = running.change(_.filterNot(_ eq task))

  private def run_registered[A](
    task: T & Task[A],
    stopped: Boolean,
    on_finish: Exn.Result[A] => Unit
  ): A =
    try {
      val result = Exn.capture { if (stopped) task.stop(); task.run() }
      on_finish(result)
      Exn.release(result)
    }
    catch { case exn: Throwable => fail(exn); throw exn }
    finally { unregister(task) }

  def fork[A](
    name: String,
    task: T & Task[A],
    daemon: Boolean = false,
    on_finish: Exn.Result[A] => Unit = (_: Exn.Result[A]) => ()
  ): Future[A] = {
    val stopped = register(task)
    try {
      Task.fork(name, new Task[A] {
        def run(): A = run_registered(task, stopped, on_finish)
        def stop(): Unit = task.stop()
      }, daemon = daemon)
    }
    catch { case exn: Throwable => unregister(task); throw exn }
  }

  def run[A](task: T & Task[A]): A = run_registered(task, register(task), _ => ())

  def join(): Unit = {
    running.guarded_access(list => if (list.isEmpty) Some(((), list)) else None)
    PIDE_MCP_Util.release_combined(
      failures_while_stopping.change_result(exns => (exns.reverse.map(Exn.Exn[Unit](_)), Nil)))
  }
}
