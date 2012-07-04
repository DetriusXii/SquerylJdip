package com.squeryl.jdip.tables

import org.squeryl.KeyedEntity

class DiplomacyUnit(val unitType: String, 
    val owner: Int, 
    val unitLocation: String,
    val unitNumber: Int,
    val gameTime: Int) extends KeyedEntity[Int] {
  val id = 0
  
  def this() = this("", 0, "", 0, GameTime.getStartGameTime.id)
}