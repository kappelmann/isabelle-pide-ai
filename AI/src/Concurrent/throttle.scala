/*  Title:      AI/src/Concurrent/throttle.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.ai

import isabelle._

object Throttle {
  sealed case class State[A](
    latest: Option[A] = None,
    last: Time = Time.zero,
    stopped: Boolean = false)
}

class Throttle[A](delay: Time, log: Logger)(f: A => Unit) {
  private val state = Synchronized(Throttle.State[A]())

  private val event = new Delay_Ops(log) {
    override def app(body: => Unit): Unit =
      Isabelle_Thread.fork(name = "pide_ai_throttle") {
        PIDE_AI_Util.log_failure(log, "Throttled event failed", Exn.capture(body))
      }
  }.first(state.value.last + delay - Time.now()) {
    synchronized {
      state.change_result(st => (st.latest, st.copy(latest = None, last = Time.now())))
        .foreach(f)
    }
  }

  def apply(a: A): Unit = {
    val accepted = state.change_result(st =>
      if (st.stopped) (false, st) else (true, st.copy(latest = Some(a))))
    if (accepted) event.invoke()
  }

  def stop(): Unit = {
    state.change(_.copy(latest = None, stopped = true))
    event.revoke()
  }
}
