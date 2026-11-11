/*  Title:      AI/src/Tools/unload_tool.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.ai

import isabelle._

class Unload_Tool extends PIDE_AI_Tool("unload") with PIDE_AI_Tool.Destructive {
  def description: String =
    "Unload nodes and their descendants. " +
    "Use only if a theory is stuck or resources must be freed."

  val nodes_arg = JSON_Schema.Arg.required("nodes", "",
    JSON_Schema.Format.list(JSON_Schema.Format.string))

  def input_schema: JSON.Object.T = JSON_Schema.object_schema(
    List(PIDE_AI_Tool_Schema.running_session_arg, nodes_arg))

  def handle(
    server: PIDE_AI_Server,
    args: JSON.Object.T,
    progress: PIDE_AI_Progress,
    context: PIDE_AI_Client_Context
  ): PIDE_AI_Tool_Result =
    PIDE_AI_Tool_Result.result {
      val session = PIDE_AI_Tool_Util.running_session_param(server.sessions, args)
      val nodes = nodes_arg.get(args)
      val node_names = nodes.map(session.resources.node_name(_, context.cwd))
      val (unloaded, change) = session.unload(node_names, progress, context.cwd)
      if (unloaded.nonEmpty) session.await_stable(change, progress)
      JSON_Object("unloaded" -> unloaded.map(session.resources.print_node_name(_, context.cwd)))
    }
}

class Unload_Tools extends PIDE_AI_Tools(new Unload_Tool)
