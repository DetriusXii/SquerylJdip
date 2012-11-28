package com.squeryl.jdip.tables
import org.squeryl.KeyedEntity
import com.squeryl.jdip.schemas.Jdip

case class PotentialMoveOrder(diplomacyUnitID: Int, moveLocationID: Int) 
	extends KeyedEntity[Int] {
  val id = 0
  def this() = this(0,0)
  
}

