/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.squeryl.jdip.tables

import org.squeryl.KeyedEntity

case class UnitType(id: String) extends KeyedEntity[String] {
  def this() = this("")
}

object UnitType {
  val ARMY = "army"
  val FLEET = "fleet"
  
  def getUnitTypes: List[UnitType] = ARMY :: FLEET :: Nil map (new UnitType(_))
}