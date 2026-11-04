/*  Title:      MCP/src/queue_lock.scala
    Author:     Kevin Kappelmann

FIFO cancellable re-entrant queue
*/

package isabelle.pide.mcp

import isabelle._

class Queue_Lock {
  private val queue = Synchronized[List[Thread]](Nil)

  def waiting: Int = queue.value.length

  def with_lock[A](
    progress: Progress,
    message: => String,
    delay: Time,
    progress_delay: Time
  )(body: => A): A = {
    val thread = Thread.currentThread().nn
    def ahead(threads: List[Thread]): Int = threads.indexOf(thread)
    def holding(threads: List[Thread]): Boolean = ahead(threads) == 0
    if (holding(queue.value)) body
    else {
      queue.change(_ ::: List(thread))
      try {
        Progress_Await.await_guarded(queue, progress, {
            val n = ahead(queue.value)
            message + if_proper(n > 0, s" ($n ahead in queue)")
          }, delay, progress_delay)(threads => Option.when(holding(threads))(()))
        body
      } finally queue.change(_.filterNot(_ == thread))
    }
  }
}
