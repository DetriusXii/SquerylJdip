/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.squeryl.jdip.tables

import org.squeryl.KeyedEntity

class OrderType(val id: String) extends KeyedEntity[String] {
  def this() = this("")
}

object OrderType {
  val MOVE = "Move"
  val SUPPORT_MOVE = "Support Move"
  val SUPPORT_HOLD = "Support Hold"
  val CONVOY = "Convoy"
  val HOLD = "HOLD"
  val CONSTRUCT = "Construct"
  val DISBAND = "Disband"
  val RETREAT = "Retreat"
  
  def getOrderTypes = 
    MOVE :: SUPPORT_MOVE :: SUPPORT_HOLD :: CONVOY :: HOLD :: CONSTRUCT :: DISBAND :: RETREAT :: Nil map
    (new OrderType(_))
}