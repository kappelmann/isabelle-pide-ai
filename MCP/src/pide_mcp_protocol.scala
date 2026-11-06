/*  Title:      MCP/src/pide_mcp_protocol.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.mcp

import isabelle._
import isabelle.pide.ai._

val PIDE_MCP_Protocol = new PIDE_AI_Protocol("mcp", new PIDE_MCP_Endpoint(_, _, _, _))

class MCP_Protocols extends PIDE_AI_Protocols(PIDE_MCP_Protocol)
