/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.squeryl.jdip.tables

import org.squeryl.KeyedEntity

case class Phase(id: String) extends KeyedEntity[String] {
  def this() = this("")
}


object Phase {
  val MOVEMENT = "Movement"
  val RETREAT = "Retreat"
  val BUILD = "Build"
  
  def getPhases: List[Phase] = MOVEMENT :: RETREAT :: BUILD :: Nil map (new Phase(_))
}
