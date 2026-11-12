/*  Title:      AI/src/Services/file_watcher_service.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.ai

import isabelle._

object File_Watcher_Service {
  def delay(options: Options): Time = options.seconds("pide_ai_file_watcher_delay")

  type Data = (PIDE_AI_File_Watcher[Document.Node.Name], PIDE_AI_Progress)
}

class File_Watcher_Service extends PIDE_AI_Service("file_watcher")
with Session_Data_Service[File_Watcher_Service.Data] {
  def description: String = "Synchronise disk changes of loaded files."

  def start_data(event: PIDE_AI_Service.Session_Event): File_Watcher_Service.Data = {
    val session = event.session
    val log = event.server.log
    val progress = new Sub_Progress(event.server.base_progress)
    val name = s"pide_ai_${id}_${session.id}"

    var loaded = (Document.Version.init, List.empty[Document.Node.Name])
    def loaded_names(): List[Document.Node.Name] = {
      val version = session.snapshot().version
      if (loaded._1 ne version) loaded = (version, version.nodes.names_iterator
        .filter(session.resources.is_loaded_node(version.nodes, _)).toList)
      loaded._2
    }

    def sync(changed: List[Document.Node.Name]): Unit =
      try {
        session.resources.sync_nodes_load_missing(session, changed.map(_ -> None),
          hide_others = false, progress)
      }
      catch {
        case exn @ Exn.Interrupt() =>
          if (!progress.stopped) log.error_message(s"${quote(name)} interrupted")
          throw exn
      }

    val watcher = new PIDE_AI_File_Watcher[Document.Node.Name](name, loaded_names, _.path, sync,
      File_Watcher_Service.delay(session.session_options), log)
    (watcher, progress)
  }

  def stop_data(event: PIDE_AI_Service.Session_Event, data: File_Watcher_Service.Data): Unit = {
    val (watcher, progress) = data
    PIDE_AI_Util.release_combined(List(
      Exn.capture { progress.stop() },
      Exn.capture { watcher.stop() }))
  }
}

class File_Watcher_Services extends PIDE_AI_Services(new File_Watcher_Service)
