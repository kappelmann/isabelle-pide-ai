/*  Title:      Tool_Extensions/src/build_document_tool.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.ai.extensions

import isabelle._
import isabelle.pide.ai._

object Build_Document_Tool {
  def build_documents(
    session: PIDE_AI_Session,
    session_name: String,
    thys: Option[List[Document.Node.Name]],
    out_dir: Path,
    progress: PIDE_AI_Progress,
    cwd: Option[Path]
  ): List[Path] = {
    def print_nodes(nodes: List[Document.Node.Name]): String =
      commas_quote(nodes.map(session.resources.print_node_name(_, cwd)))

    val background =
      Document_Build.session_background(session.session_options, session_name, dirs = session.dirs)
    if (background.info.documents.isEmpty)
      error(s"Session ${quote(session_name)} defines no document: " +
        "its ROOT needs option document=pdf and document_files")
    val document_thys = background.base.all_document_theories
    val nodes =
      thys.getOrElse(document_thys.map(thy => session.resources.node_name(thy.path.implode, cwd)))
    val unknown = nodes.filterNot(node => document_thys.exists(_.theory == node.theory))
    if (unknown.nonEmpty) error(s"Not document theories of ${quote(session_name)}: " +
      print_nodes(unknown))

    val snapshot = session.snapshot()
    val (unloaded, loaded) = nodes.filterNot(session.resources.is_base_session_theory)
      .partition(node => !PIDE_AI_Nodes.is_loaded(snapshot.version.nodes, node))
    if (unloaded.nonEmpty) error("Theories not loaded: " + print_nodes(unloaded))
    val pending = loaded.filterNot(node => snapshot.theory_consolidated(node.theory))
    if (pending.nonEmpty) error("Theories still pending: " + print_nodes(pending))

    using(session.open_session_context(document_snapshot = Some(snapshot))) { session_context =>
      Document_Build.build_documents(
        Document_Build.context(session_context,
          document_session = Some(background.base),
          document_selection = nodes.map(_.theory).toSet, progress = progress),
        output_sources = Some(out_dir)).map(_.write(out_dir))
    }
  }
}

class Build_Document_Tool extends PIDE_AI_Tool("build_document") {
  def description: String =
    "Build LaTeX and PDF document for a session's theories, loaded in the running session."

  val session_name_arg = JSON_Schema.Arg.required("session_name", "", JSON_Schema.Format.string)
  val thys_arg = JSON_Schema.Arg.optional("thys", "Included theories. Default: all.",
    JSON_Schema.Format.list(JSON_Schema.Format.string))
  val out_dir_arg = JSON_Schema.Arg.default("out_dir", "", JSON_Schema.Format.string,
    Options.init().check_name("document_output").standard_value.get)

  def input_schema: JSON.Object.T = JSON_Schema.object_schema(
    List(PIDE_AI_Tool_Schema.running_session_arg, session_name_arg, thys_arg, out_dir_arg))

  def handle(
    server: PIDE_AI_Server,
    args: JSON.Object.T,
    progress: PIDE_AI_Progress,
    context: PIDE_AI_Client_Context
  ): PIDE_AI_Tool_Result =
    PIDE_AI_Tool_Result.result {
      val session = PIDE_AI_Tool_Util.running_session_param(server.sessions, args)
      val cwd = context.cwd
      Build_Document_Tool.build_documents(session, session_name_arg.get(args),
        thys_arg.get(args).map(_.map(session.resources.node_name(_, cwd))),
        PIDE_AI_Util.path(out_dir_arg.get(args), cwd), progress, cwd)
      .map(PIDE_AI_Util.print_path(_, cwd))
    }
}

class Build_Document_Tools extends PIDE_AI_Tools(new Build_Document_Tool)
