/*  Title:      AI/src/Tools/create_scratch_tool.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.ai

import isabelle._

import java.io.{File => JFile}

object Create_Scratch_Tool {
  type Data = Synchronized[Option[JFile]]
}

class Create_Scratch_Tool extends PIDE_AI_Tool("create_scratch")
with Session_Data_Service[Create_Scratch_Tool.Data] {
  def description: String =
    "Create a temporary file for experimentation. " +
    "Files are cleaned up on session stop."

  val name_suffix_arg = JSON_Schema.Arg.optional(
    "name_suffix", "Label to identify the file. Default: auto-generated.",
    JSON_Schema.Format.string)
  val extension_arg = JSON_Schema.Arg.default(
    "extension", "", JSON_Schema.Format.string, ".thy")

  def input_schema: JSON.Object.T = JSON_Schema.object_schema(
    List(PIDE_AI_Tool_Schema.running_session_arg, name_suffix_arg, extension_arg))

  private val file_prefix: String = "Scratch_"
  private val dir_prefix: String = "tmp_pide_ai_scratch"

  def start_data(event: PIDE_AI_Service.Session_Event): Create_Scratch_Tool.Data =
    Synchronized(None)

  def stop_data(event: PIDE_AI_Service.Session_Event, data: Create_Scratch_Tool.Data): Unit =
    data.change { dir => dir.foreach(Isabelle_System.rm_tree); None }

  def create_scratch(
    session: PIDE_AI_Session,
    name_suffix: Option[String],
    extension: String
  ): (Path, Boolean) = {
    def not_running: Nothing =
      error(s"Cannot create scratch file: session ${quote(session.id)} is not running")
    val scratch_dir = data(session).getOrElse(not_running)
    scratch_dir.change_result { current =>
      if (!data(session).contains(scratch_dir)) not_running
      val dir = current.filter(_.isDirectory).getOrElse(Isabelle_System.tmp_dir(dir_prefix))
      def file_path(suffix: String): Path =
        File.path(dir) + Path.basic(file_prefix + suffix + extension)
      val (path, created) = name_suffix match {
        case Some(suffix) =>
          val path = file_path(suffix)
          (path, path.file.createNewFile())
        case None =>
          (Iterator.from(1).map(i => file_path(i.toString)).find(_.file.createNewFile()).get, true)
      }
      ((path.canonical, created), Some(dir))
    }
  }

  def handle(
    server: PIDE_AI_Server,
    args: JSON.Object.T,
    progress: PIDE_AI_Progress,
    context: PIDE_AI_Client_Context
  ): PIDE_AI_Tool_Result =
    PIDE_AI_Tool_Result.result {
      val session = PIDE_AI_Tool_Util.running_session_param(server.sessions, args)
      val name_suffix = name_suffix_arg.get(args)
      val extension = extension_arg.get(args)
      val (path, created) = create_scratch(session, name_suffix, extension)
      JSON_Object("path" -> PIDE_AI_Util.print_path(path, context.cwd)) ++
      JSON_Object.if_proper(!created, JSON_Object("note" -> "File already existed."))
    }
}

class Create_Scratch_Tools extends PIDE_AI_Tools(new Create_Scratch_Tool)
