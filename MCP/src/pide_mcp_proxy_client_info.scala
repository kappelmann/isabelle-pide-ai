/*  Title:      MCP/src/pide_mcp_proxy_client_info.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.mcp

import isabelle._

// TODO: update once non-localhost addresses are allowed
val PIDE_MCP_Proxy_Client_Info = new PIDE_AI_Client_Info("pide_mcp_proxy", PIDE_MCP_Protocol, {
    case tcp: PIDE_AI_TCP_Connector => Some(
      "MCP: " + quote(File.platform_path(Path.explode("$ISABELLE_HOME/bin/isabelle"))) +
      s" pide_mcp -p ${tcp.port} -s.")
    case _ => None
  })

class MCP_Proxy_Client_Infos extends PIDE_AI_Client_Infos(PIDE_MCP_Proxy_Client_Info)
