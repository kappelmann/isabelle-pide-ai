/*  Title:      AI/src/General/pide_ai_file_watcher.scala
    Author:     Kevin Kappelmann

File watcher (NB. Isabelle's file watcher for MacOS has no configurable polling interval).
*/

package isabelle.pide.ai

import isabelle._

import java.io.{File => JFile}
import java.nio.file.Files
import java.nio.file.attribute.BasicFileAttributes

object PIDE_AI_File_Watcher {
  type Stamp = (Long, Long)

  def stamp(file: JFile): Option[Stamp] =
    try {
      val attrs = Files.readAttributes(file.toPath, classOf[BasicFileAttributes]).nn
      if (attrs.isRegularFile) Some((attrs.lastModifiedTime.nn.toMillis, attrs.size)) else None
    }
    catch { case ERROR(_) => None }
}

class PIDE_AI_File_Watcher[A](
  name: String,
  files: () => Iterable[A],
  path: A => Path,
  changed: List[A] => Unit,
  delay: Time,
  log: Logger
) {
  private var stamps = Map.empty[A, (JFile, PIDE_AI_File_Watcher.Stamp)]

  private def check(): Unit = {
    val current = (for {
      a <- files().iterator
      file = stamps.get(a).fold(path(a).file)(_._1)
      stamp <- PIDE_AI_File_Watcher.stamp(file)
    } yield a -> (file, stamp)).toMap
    val changes = current.iterator.collect {
      case (a, (_, stamp)) if !stamps.get(a).exists(_._2 == stamp) => a
    }.toList
    if (changes.nonEmpty) changed(changes)
    stamps = current
  }

  private val consumer = Consumer_Thread.fork_bulk[Unit](name, daemon = true, log = log,
    timeout = Some(delay),
    consume = { _ =>
      val result = Exn.capture_trace(log.error_message, prefix = s"${quote(name)} stopped")(check())
      (Nil, Exn.is_res(result))
    })

  def stop(): Unit = consumer.shutdown()
}
