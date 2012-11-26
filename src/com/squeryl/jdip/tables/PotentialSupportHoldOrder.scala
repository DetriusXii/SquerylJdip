package com.squeryl.jdip.tables
import org.squeryl.KeyedEntity

case class PotentialSupportHoldOrder(diplomacyUnitID: Int, 
    supportHoldLocationID: Int) extends KeyedEntity[Int] {
  val id = 0
  
  def this() = this(0,0)
}