/*  Title:      AI/src/Tools/export_tool.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.ai

import isabelle._

object Export_Tool {
  def write_export(entry: Export.Entry, dir: Path, progress: Progress): Path = {
    progress.expose_interrupt()
    val path = dir + entry.entry_name.make_path()
    progress.echo("export " + path + (if (entry.executable) " (executable)" else ""))
    Isabelle_System.make_directory(path.dir)
    val bytes = entry.bytes
    if (!path.is_file || Bytes.read(path) != bytes) Bytes.write(path, bytes)
    File.set_executable(path, reset = !entry.executable)
    path
  }

  val node_exports: PIDE_AI_Node_Cache.Data[List[Export.Entry]] =
    PIDE_AI_Node_Cache.Data(_.exports)

  def exports(session: PIDE_AI_Session): List[Export.Entry] = {
    val view = session.node_cache.view(session.snapshot())
    view.snapshot.version.nodes.iterator.collect { case (name, _) if name.is_theory => name }
      .flatMap(view(node_exports, _)).toList
      .sortBy(entry => (entry.entry_name.theory, entry.entry_name.name))
  }

  def write_exports(
    session: PIDE_AI_Session,
    patterns: List[String],
    out_dir: Path,
    progress: Progress
  ): List[Path] = {
    val matcher = Export.make_matcher(patterns)
    exports(session).filter(entry => matcher(entry.entry_name))
      .map(write_export(_, out_dir, progress))
  }
}

class Export_Tool extends PIDE_AI_Tool("export") {
  def description: String = "List or write exports (e.g. code)."

  val patterns_arg = JSON_Schema.Arg.optional("patterns",
    s"Write exports matching THEORY:NAME patterns (e.g. ${quote("Scratch:code/**")}). " +
      "Default: list exports.",
    JSON_Schema.Format.list(JSON_Schema.Format.string))
  val out_dir_arg = JSON_Schema.Arg.default("out_dir", "", JSON_Schema.Format.string,
    Export.default_export_dir.implode)

  def input_schema: JSON.Object.T = JSON_Schema.object_schema(
    List(PIDE_AI_Tool_Schema.running_session_arg, patterns_arg, out_dir_arg))

  def handle(
    server: PIDE_AI_Server,
    args: JSON.Object.T,
    progress: PIDE_AI_Progress,
    context: PIDE_AI_Client_Context
  ): PIDE_AI_Tool_Result =
    PIDE_AI_Tool_Result.result {
      val session = PIDE_AI_Tool_Util.running_session_param(server.sessions, args)
      val cwd = context.cwd
      patterns_arg.get(args) match {
        case None => Export_Tool.exports(session).map(_.compound_name)
        case Some(patterns) =>
          Export_Tool.write_exports(session, patterns,
            PIDE_AI_Util.path(out_dir_arg.get(args), cwd), progress)
          .map(PIDE_AI_Util.print_path(_, cwd))
      }
    }
}

class Export_Tools extends PIDE_AI_Tools(new Export_Tool)
