/*  Title:      AI/src/Command/pide_ai_command_protocol.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.ai

import isabelle._

val PIDE_AI_Command_Protocol = new PIDE_AI_Protocol("cmd", new PIDE_AI_Command_Endpoint(_, _, _, _))

class Command_Protocols extends PIDE_AI_Protocols(PIDE_AI_Command_Protocol)
