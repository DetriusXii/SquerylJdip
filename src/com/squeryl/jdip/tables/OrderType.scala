/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.squeryl.jdip.tables

import org.squeryl.KeyedEntity

case class OrderType(id: String, phase: String) extends KeyedEntity[String] {
  def this() = this("", "")
}

object OrderType {
  val MOVE = "Move"
  val SUPPORT_MOVE = "SupportMove"
  val SUPPORT_HOLD = "SupportHold"
  val CONVOY = "Convoy"
  val HOLD = "Hold"
  val CONSTRUCT = "Construct"
  val DISBAND = "Disband"
  val RETREAT = "Retreat"    
    
  def getOrderTypes = (MOVE, Phase.MOVEMENT) :: 
		(SUPPORT_MOVE, Phase.MOVEMENT) :: 
	    (SUPPORT_HOLD, Phase.MOVEMENT) :: 
	    (HOLD, Phase.MOVEMENT) :: 
	    (CONVOY, Phase.MOVEMENT) ::
	    (CONSTRUCT, Phase.BUILD) :: 
	    (DISBAND, Phase.BUILD) :: 
	    (RETREAT, Phase.RETREAT) :: Nil map ((u: Tuple2[String, String]) => 
	    	new OrderType(u._1, u._2)
	    )
}