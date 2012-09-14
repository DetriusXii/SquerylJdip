package com.squeryl.jdip.tables

import org.squeryl.KeyedEntity

case class DiplomacyUnit(unitType: String, 
    owner: Int, 
    unitLocation: Int,
    unitNumber: Int,
    gameTime: Int) extends KeyedEntity[Int] {
  val id = 0
  
  def this() = this("", 0, 0, 0, 0)
}