/*  Title:      AI/src/Services/file_watcher_service.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.ai

import isabelle._

object File_Watcher_Service {
  def delay(options: Options): Time = options.seconds("pide_ai_file_watcher_delay")
}

class File_Watcher_Service extends PIDE_AI_Service("file_watcher") {
  def description: String = "Synchronise disk changes of loaded files."

  private val watchers = Synchronized(
    Map.empty[String, (PIDE_AI_File_Watcher[Document.Node.Name], PIDE_AI_Progress)])

  private def loaded_files(session: PIDE_AI_Session): () => List[Document.Node.Name] = {
    var cache = (Document.Version.init, List.empty[Document.Node.Name])
    () => {
      val version = session.snapshot().version
      if (cache._1 ne version) cache = (version, version.nodes.iterator.collect {
        case (name, _) if PIDE_AI_Nodes.is_loaded(version.nodes, name) => name
      }.toList)
      cache._2
    }
  }

  override def started(event: PIDE_AI_Service.Session_Event): Unit = {
    val session = event.session
    val log = event.server.log
    val sync_progress = new Sub_Progress(event.server.base_progress)
    val watcher = new PIDE_AI_File_Watcher[Document.Node.Name](
      session.id, loaded_files(session), _.path,
      changed => Exn.capture { session.sync_loaded_resolve(changed, sync_progress) } match {
        case Exn.Exn(exn) if Exn.is_interrupt(exn) =>
          if (!sync_progress.stopped)
            log.error_message(s"File watcher ${quote(session.id)} interrupted")
          throw exn
        case Exn.Exn(exn) =>
          log.warning(s"File watcher ${quote(session.id)} failed to synchronize: " +
            Exn.message(exn))
        case _ =>
      },
      File_Watcher_Service.delay(session.session_options), log)
    watchers.change(_ + (session.id -> (watcher, sync_progress)))
  }

  override def stopping(event: PIDE_AI_Service.Session_Event): Unit = {
    val id = event.session.id
    watchers.change_result(watchers => (watchers.get(id), watchers - id))
      .foreach { (watcher, sync_progress) => sync_progress.stop(); watcher.shutdown() }
  }
}

class File_Watcher_Services extends PIDE_AI_Services(new File_Watcher_Service)
