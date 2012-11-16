package com.squeryl.jdip.tables

import org.squeryl.KeyedEntity
import com.squeryl.jdip.schemas.Jdip
import org.squeryl.dsl.ManyToOne

case class DiplomacyUnit(unitType: String, 
    gamePlayerEmpireID: Int, 
    unitLocationID: Int,
    unitNumber: Int,
    gameTimeID: Int) extends KeyedEntity[Int] {
  val id = 0
  
  def this() = this("", 0, 0, 0, 0)
  
  lazy val gamePlayerEmpire: ManyToOne[GamePlayerEmpire] = Jdip.dpuOwnerForeignKey.right(this)
  lazy val unitLocation: ManyToOne[Location] = Jdip.dpuLocationForeignKey.right(this)
  lazy val gameTime: ManyToOne[GameTime] = Jdip.dpuGameTimeForeignKey.right(this)
}