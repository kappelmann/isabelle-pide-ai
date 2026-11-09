/*  Title:      AI/src/Concurrent/queue_lock.scala
    Author:     Kevin Kappelmann

FIFO cancellable re-entrant queue
*/

package isabelle.pide.ai

import isabelle._

object Queue_Lock {
  def ahead_text(ahead: Int): String = s"($ahead ahead in queue)"
}

class Queue_Lock {
  private val queue = Synchronized[List[Thread]](Nil)

  def waiting: Int = queue.value.length

  def with_lock[A](
    progress: Progress,
    message: Time => String,
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
        PIDE_AI_Progress.await_guarded(queue, progress, { elapsed =>
            val n = ahead(queue.value)
            message(elapsed) + if_proper(n > 0, " " + Queue_Lock.ahead_text(n))
          }, delay, progress_delay)(threads => Option.when(holding(threads))(()))
        body
      } finally queue.change(_.filterNot(_ == thread))
    }
  }
}
