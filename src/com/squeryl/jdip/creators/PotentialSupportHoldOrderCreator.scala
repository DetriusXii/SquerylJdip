package com.squeryl.jdip.creators
import com.squeryl.jdip.queries.DBQueries
import com.squeryl.jdip.tables._

class PotentialSupportHoldOrderCreator(game: Game) {
  def getSupportHolds(diplomacyUnit: DiplomacyUnit): List[Location] = {
    val unitLocation = DBQueries.getLocationForDiplomacyUnit(diplomacyUnit)
    val adjacentLocations = 
      DBQueries.getAdjacentLocationsForLocation(unitLocation)
    val provincialLocations =
      DBQueries.locations.filter(_ match {
        case Location(province, _) => 
          adjacentLocations.exists(_.province.equals(province))
      })
    val allUnits = DBQueries.getDiplomacyUnitsForGameAtCurrentGameTime(game)
    val provincialLocationsWithUnitPresent =
      provincialLocations.filter(loc => allUnits.exists(_.unitLocation == loc.id))
    provincialLocationsWithUnitPresent
  }
  
  def createPotentialSupportHoldOrders(): List[PotentialSupportHoldOrder] = {
    val dpusForGame = 
      DBQueries.getDiplomacyUnitsForGameAtCurrentGameTime(game)
      
    dpusForGame.foldLeft(Nil: List[PotentialSupportHoldOrder])((u, v) => {
      val supportHoldsForUnit = getSupportHolds(v)
      supportHoldsForUnit.map(w => new PotentialSupportHoldOrder(v.id, w.id))
    })
  }
}