/*  Title:      AI/src/Concurrent/throttle.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.ai

import isabelle._

object Throttle {
  sealed case class State[A](
    latest: Option[A] = None,
    last: Time = Time.zero,
    active: Boolean = false,
    stopped: Boolean = false)
}

class Throttle[A](delay: Time, log: Logger)(f: A => Unit) {
  private val state = Synchronized(Throttle.State[A]())

  private val event: Delay = new Delay_Ops(log) {
    override def app(body: => Unit): Unit =
      Isabelle_Thread.fork(name = "pide_ai_throttle") {
        PIDE_AI_Util.log_failure(log, "Throttled event failed", Exn.capture(body))
      }
  }.first(state.value.last + delay - Time.now()) {
    try {
      state.change_result(st => (st.latest, st.copy(latest = None, last = Time.now())))
        .foreach(f)
    }
    finally {
      val again = state.change_result(st =>
        if (st.latest.isDefined && !st.stopped) (true, st) else (false, st.copy(active = false)))
      if (again) event.invoke()
    }
  }

  def apply(a: A): Unit = {
    val start = state.change_result(st =>
      if (st.stopped) (false, st) else (!st.active, st.copy(latest = Some(a), active = true)))
    if (start) event.invoke()
  }

  def stop(): Unit = {
    state.change(_.copy(latest = None, stopped = true))
    event.revoke()
  }
}
