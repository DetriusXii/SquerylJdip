package com.squeryl.jdip.tables

import org.squeryl.KeyedEntity
import org.squeryl.dsl._
import org.squeryl.PrimitiveTypeMode._
import com.squeryl.jdip.schemas.Jdip

case class OwnedProvince(provinceID: String, 
    gamePlayerEmpireID: Int,
    gameTimeID: Int) extends
	KeyedEntity[CompositeKey3[String, Int, Int]] {
  
  def this() = this("", 0, 0)
  def id = compositeKey(provinceID, gamePlayerEmpireID, gameTimeID)
  
  lazy val gamePlayerEmpire = Jdip.owpGamePlayerEmpireForeignKey.right(this)
  lazy val gameTime = Jdip.owpGameTimeForeignKey.right(this)
  lazy val province = Jdip.owpProvinceForeignKey.right(this)
}

object OwnedProvince {
  def getOwnedProvinces(diplomacyUnits: List[DiplomacyUnit],
      locations: List[Location]): List[OwnedProvince] = 
        diplomacyUnits.map(dpu => {
          val unitLocation: Option[Location] = 
            locations.find(loc => loc.id == dpu.unitLocationID)
          unitLocation.map(_ match {
            case Location(province, _) => 
              OwnedProvince(province, dpu.gamePlayerEmpireID, dpu.gameTimeID)
          })
        }).flatten
}