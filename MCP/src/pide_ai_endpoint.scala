/*  Title:      MCP/src/pide_ai_endpoint.scala
    Author:     Kevin Kappelmann

Server side of one connection.
*/

package isabelle.pide.mcp

import isabelle._

abstract class PIDE_AI_Endpoint(
  val server: PIDE_AI_Server,
  val connection: Line_Connection
) extends Task[Unit]
