/*  Title:      AI/src/Tools/create_file_tool.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.ai

import isabelle._

object Create_File_Tool {
  def create_file(path: Path): Path = {
    val abs_path = path.canonical
    Isabelle_System.make_directory(abs_path.dir)
    if (!abs_path.file.createNewFile()) {
      val kind = if (abs_path.file.isDirectory) "directory" else "path"
      error(s"Path ${quote(abs_path.implode)} is an existing $kind")
    }
    abs_path
  }
}

class Create_File_Tool extends PIDE_AI_Tool("create_file") {
  def description: String =
    "Create an empty file (and parent directories) at the given path."
  val path_arg = JSON_Schema.Arg.required("path", "", JSON_Schema.Format.string)

  def input_schema: JSON.Object.T = JSON_Schema.object_schema(List(path_arg))

  def handle(
    server: PIDE_AI_Server,
    args: JSON.Object.T,
    progress: Progress
  ): PIDE_AI_Tool_Result =
    PIDE_AI_Tool_Result.result {
      val file_path = path_arg.get(args)
      JSON_Object("path" -> Create_File_Tool.create_file(PIDE_AI_Util.path(file_path)).implode)
    }
}

class Create_File_Tools extends PIDE_AI_Tools(new Create_File_Tool)
