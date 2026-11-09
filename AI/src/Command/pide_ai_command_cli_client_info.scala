/*  Title:      AI/src/Command/pide_ai_command_cli_client_info.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.ai

import isabelle._

// TODO: update once non-localhost addresses are allowed
val PIDE_AI_Command_CLI_Client_Info = new PIDE_AI_Client_Info("pide_ai_cli",
  PIDE_AI_Command_Protocol, {
    case tcp: PIDE_AI_TCP_Connector => Some(
      s"CLI on port ${tcp.port}: `CLIENT -?` shows usage (clients in " +
      quote(File.platform_path(Path.explode("$PIDE_AI_HOME/clients"))) + ").")
    case _ => None
  })

class Command_Client_Infos extends PIDE_AI_Client_Infos(PIDE_AI_Command_CLI_Client_Info)
