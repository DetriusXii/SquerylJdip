package com.squeryl.jdip.tables

import org.squeryl.KeyedEntity
import org.squeryl.dsl._
import org.squeryl.PrimitiveTypeMode._

case class OwnedProvince(province: String, 
    gamePlayerEmpireID: Int,
    gameTimeID: Int) extends
	KeyedEntity[CompositeKey3[String, Int, Int]] {
  
  def this() = this("", 0, 0)
  def id = compositeKey(province, gamePlayerEmpireID, gameTimeID)
}

object OwnedProvince {
  def getOwnedProvinces(diplomacyUnits: Iterable[DiplomacyUnit],
      locations: Iterable[Location]): Iterable[OwnedProvince] = 
        diplomacyUnits.map(dpu => {
          val unitLocation: Option[Location] = 
            locations.find(loc => loc.id == dpu.unitLocation)
          unitLocation.map(_ match {
            case Location(province, _) => OwnedProvince(province, dpu.owner, dpu.gameTime)
          })
        }).flatten
}