/*  Title:      AI/src/General/progress.scala
    Author:     Kevin Kappelmann

Progress wrappers and utilities.
*/

package isabelle.pide.ai

import isabelle._

import scala.annotation.tailrec

object Progress_Await {
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
}

class Silent_Progress(progress: Progress) extends Progress {
  override def verbose: Boolean = progress.verbose
  override def stopped: Boolean = progress.stopped
  override def stop(): Unit = progress.stop()
  override def interrupt_handler[A](e: => A): A = progress.interrupt_handler(e)
  override def output(messages: Progress.Output): Unit = ()
  override def nodes_status(status: Progress.Nodes_Status): Unit = ()
}

class Sub_Progress(progress: Progress) extends Progress {
  override def verbose: Boolean = progress.verbose
  override def stopped: Boolean = super.stopped || progress.stopped
  override def interrupt_handler[A](e: => A): A = progress.interrupt_handler(e)
  override def output(messages: Progress.Output): Unit = progress.output(messages)
  override def nodes_status(status: Progress.Nodes_Status): Unit = progress.nodes_status(status)
}

class Reporting_Progress(
  base_progress: Progress.Status,
  report_progress: Option[(Long, String) => Unit]
) extends Sub_Progress(base_progress) with Progress.Local_Interrupts {
  private var serial: Long = 0L
  private var reporting = true

  def stop_reporting(): Unit = synchronized { reporting = false }

  private def report_messages(messages: Progress.Output): Unit =
    for {
      report <- report_progress if reporting
      message <- messages if do_output(message)
    } {
      serial += 1
      report(serial, message.message.output_text)
    }

  override def output(messages: Progress.Output): Unit = synchronized {
    super.output(messages)
    report_messages(messages)
  }

  private def theories_summary(status: Progress.Nodes_Status): Option[Progress.Msg] = {
    val theories = status.domain.map(name => name.theory -> status(name))
    Option.when(theories.nonEmpty) {
      val finished = theories.count({ case (_, st) => st.percentage == 100 })
      val running = for ((theory, st) <- theories if st.progress)
        yield theory + " " + st.percentage + "%"
      Progress.Message(Output.Kind.writeln,
        if_proper(status.session, status.session + ": ") +
        s"${finished}/${theories.length} theories finished" +
        if_proper(running, ", running: " + commas(running)), verbose = false)
    }
  }

  override def nodes_status(status: Progress.Nodes_Status): Unit = synchronized {
    super.nodes_status(status)
    report_messages(theories_summary(status).toList :::
      status.long_running_commands(base_progress.status_threshold).map(_.copy(verbose = false)))
  }
}

class Unstoppable_Progress(progress: Progress) extends Progress {
  override def verbose: Boolean = progress.verbose
  override def stopped: Boolean = false
  override def stop(): Unit = ()
  override def output(messages: Progress.Output): Unit = progress.output(messages)
  override def nodes_status(status: Progress.Nodes_Status): Unit = progress.nodes_status(status)
}

class Console_File_Progress(
  path: Path,
  override val verbose: Boolean = false,
  threshold: Time,
  detailed: Boolean = false,
  stderr: Boolean = true)
extends Progress with Progress.Global_Interrupts with Progress.Status {
  private val console = new Console_Progress(verbose = verbose, stderr = stderr)
  private val file = new File_Progress(path, verbose = verbose)

  override def status_threshold: Time = threshold
  override def status_detailed: Boolean = detailed
  override def status_output(msgs: Progress.Output): Unit = {
    console.status_output(msgs)
    file.status_output(msgs)
  }
  override def status_hide(msgs: Progress.Output): Unit = {
    console.status_hide(msgs)
    file.status_hide(msgs)
  }
}
