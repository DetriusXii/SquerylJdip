package com.squeryl.jdip.tables
import org.squeryl.KeyedEntity

case class PotentialSupportMoveOrder(diplomacyUnitID: Int, 
    supportMoveTargetLocationID: Int, supportMoveSourceLocationID: Int) 
    extends KeyedEntity[Int] {
  val id = 0
  def this() = this(0,0,0)
}