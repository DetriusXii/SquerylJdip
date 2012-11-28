package com.squeryl.jdip.tables
import org.squeryl.KeyedEntity
import com.squeryl.jdip.schemas.Jdip

case class PotentialSupportMoveOrder(diplomacyUnitID: Int, 
    supportMoveTargetLocationID: Int, supportMoveSourceLocationID: Int) 
    extends KeyedEntity[Int] {
  val id = 0
  def this() = this(0,0,0)
  
  lazy val diplomacyUnit = Jdip.psmoDiplomacyUnitForeignKey.right(this)
  lazy val supportMoveTargetLocation = 
    Jdip.psmoTargetLocationForeignKey.right(this)
  lazy val supportMoveSourceLocation = 
    Jdip.psmoSourceLocationForeignKey.right(this)
}