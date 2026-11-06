/*  Title:      AI/src/Server/pide_ai_endpoint.scala
    Author:     Kevin Kappelmann

Server side of one connection.
*/

package isabelle.pide.ai

import isabelle._

abstract class PIDE_AI_Endpoint(
  val server: PIDE_AI_Server,
  val connection: Line_Connection,
  val context: PIDE_AI_Client_Context
) extends Task[Unit]
