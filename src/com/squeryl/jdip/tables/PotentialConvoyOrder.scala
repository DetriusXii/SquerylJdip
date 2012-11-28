package com.squeryl.jdip.tables
import org.squeryl.KeyedEntity
import com.squeryl.jdip.schemas.Jdip

case class PotentialConvoyOrder(diplomacyUnitID: Int, 
    convoyTargetLocationID: Int, 
    convoySourceLocationID: Int) extends KeyedEntity[Int] {
  val id = 0
  def this() = this(0,0,0)
  
  lazy val diplomacyUnit = Jdip.pcoDiplomacyUnitForeignKey.right(this)
  lazy val convoyTargetLocation = Jdip.pcoTargetLocationForeignKey.right(this)
  lazy val convoySourceLocation = Jdip.pcoSourceLocationForeignKey.right(this)
}