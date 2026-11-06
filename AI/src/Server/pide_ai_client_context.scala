/*  Title:      AI/src/Server/pide_ai_client_context.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.ai

import isabelle._

object PIDE_AI_Client_Context {
  def apply(dir: Option[Path]): PIDE_AI_Client_Context =
    new PIDE_AI_Client_Context(dir.map(dir => if (dir.is_absolute && dir.is_dir) dir.canonical
      else error(s"Working directory ${quote(dir.implode)} is not an absolute path " +
        "to an existing directory")))

  def current_path: PIDE_AI_Client_Context = PIDE_AI_Client_Context(Some(Path.current.absolute))
}

class PIDE_AI_Client_Context private(val cwd: Option[Path])
