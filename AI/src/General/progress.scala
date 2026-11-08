/*  Title:      AI/src/General/progress.scala
    Author:     Kevin Kappelmann

Progress wrappers and utilities.
*/

package isabelle.pide.ai

import isabelle._

import scala.annotation.tailrec

object PIDE_AI_Progress {
  def await[A](
    progress: Progress,
    message: => String,
    progress_delay: Time
  )(attempt: => Option[A]): A = {
    val start = Time.now()
    @tailrec def poll(limit: Time): A = {
      progress.expose_interrupt()
      attempt match {
        case Some(result) => result
        case None =>
          val now = Time.now()
          val limit1 =
            if (now < limit) limit
            else {
              progress.echo(message + s" ... (${(now - start).message} elapsed time)")
              now + progress_delay
            }
          poll(limit1)
      }
    }
    poll(Time.now() + progress_delay)
  }

  def await_delay[A](
    progress: Progress,
    message: => String,
    delay: Time,
    progress_delay: Time
  )(value: => Option[A]): A =
    await(progress, message, progress_delay)(value.orElse { delay.sleep(); None })

  def await_guarded[S, A](
    state: Synchronized[S],
    progress: Progress,
    message: => String,
    delay: Time,
    progress_delay: Time
  )(value: S => Option[A]): A =
    await(progress, message, progress_delay)(
      state.timed_access(_ => Some(Time.now() + delay), s => value(s).map(_ -> s)))

  def theories_progress_msg(status: Progress.Nodes_Status): Option[Progress.Msg] = {
    val theories = status.domain.map(name => name.theory -> status(name))
    Option.when(theories.nonEmpty) {
      val finished = theories.count((_, st) => st.percentage == 100)
      val running = for ((theory, st) <- theories if st.progress)
        yield theory + " " + st.percentage + "%"
      Progress.Message(Output.Kind.writeln,
        if_proper(status.session, status.session + ": ") +
        s"${finished}/${theories.length} theories finished" +
        if_proper(running, ", running: " + commas(running)), verbose = false)
    }
  }

  def long_running_output(threshold: Time)(status: Progress.Nodes_Status): Progress.Output =
    status.long_running_commands(threshold).map(_.copy(verbose = false))

  def progress_long_running_output(threshold: Time)(
    status: Progress.Nodes_Status
  ): Progress.Output =
    theories_progress_msg(status).toList ::: long_running_output(threshold)(status)
}

abstract class Wrapped_Progress(progress: Progress) extends Progress {
  override def now(): Date = progress.now()
  override def verbose: Boolean = progress.verbose
  override def stopped: Boolean = progress.stopped
  override def stop(): Unit = progress.stop()
  override def interrupt_handler[A](e: => A): A = progress.interrupt_handler(e)
  override def output(messages: Progress.Output): Unit = progress.output(messages)
  override def nodes_status(status: Progress.Nodes_Status): Unit = progress.nodes_status(status)
}

class Silent_Progress(progress: Progress) extends Wrapped_Progress(progress) {
  override def output(messages: Progress.Output): Unit = ()
  override def nodes_status(status: Progress.Nodes_Status): Unit = ()
}

class Recode_Progress(progress: Progress, unicode_symbols: Boolean)
extends Wrapped_Progress(progress) {
  override def output(messages: Progress.Output): Unit =
    super.output(messages.map {
      case msg: Progress.Message => msg.copy(text = Symbol.output(unicode_symbols, msg.text))
      case msg => msg
    })
}

class Sub_Progress(progress: Progress) extends Wrapped_Progress(progress) {
  @volatile private var is_stopped = false
  override def stop(): Unit = { is_stopped = true }
  override def stopped: Boolean = is_stopped || progress.stopped
}

class Reporting_Progress(
  progress: Progress,
  report_output: Progress.Output => Unit = _ => (),
  report_nodes_status: Progress.Nodes_Status => Unit = _ => ()
) extends Wrapped_Progress(progress) {
  private var reporting = true

  def stop_reporting(): Unit = synchronized { reporting = false }

  override def output(messages: Progress.Output): Unit = synchronized {
    super.output(messages)
    if (reporting) report_output(messages.filter(do_output))
  }

  override def nodes_status(status: Progress.Nodes_Status): Unit = synchronized {
    super.nodes_status(status)
    if (reporting) report_nodes_status(status)
  }
}

class Unstoppable_Progress(progress: Progress) extends Wrapped_Progress(progress) {
  override def stopped: Boolean = false
  override def stop(): Unit = ()
  override def interrupt_handler[A](e: => A): A = e
}

class Tee_Progress(progresses: Progress*) extends Progress {
  override def verbose: Boolean = progresses.exists(_.verbose)
  override def output(messages: Progress.Output): Unit = progresses.foreach(_.output(messages))
  override def nodes_status(status: Progress.Nodes_Status): Unit =
    progresses.foreach(_.nodes_status(status))
}
