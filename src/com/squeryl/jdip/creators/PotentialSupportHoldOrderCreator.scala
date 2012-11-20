package com.squeryl.jdip.creators
import com.squeryl.jdip.queries.DBQueries
import com.squeryl.jdip.tables._

class PotentialSupportHoldOrderCreator(game: Game, dbQueries: DBQueries) {
  def getSupportHolds(diplomacyUnit: DiplomacyUnit): List[Location] = {
    val unitLocation = dbQueries.getLocationFromDiplomacyUnit(diplomacyUnit)
    val adjacentLocations = 
      dbQueries.getAdjacentLocationsForLocation(unitLocation)
    val provincialLocations =
      dbQueries.locations.filter(_ match {
        case Location(province, _) => 
          adjacentLocations.exists(_.province.equals(province))
      })
    val allUnits = dbQueries.getDiplomacyUnitsForGameAtCurrentGameTime(game)
    val provincialLocationsWithUnitPresent =
      provincialLocations.filter(loc => allUnits.exists(_.unitLocation == loc.id))
    provincialLocationsWithUnitPresent
  }
  
  def createPotentialSupportHoldOrders(): List[PotentialSupportHoldOrder] = {
    val dpusForGame = 
      dbQueries.getDiplomacyUnitsForGameAtCurrentGameTime(game)
      
    dpusForGame.foldLeft(Nil: List[PotentialSupportHoldOrder])((u, v) => {
      val supportHoldsForUnit = getSupportHolds(v)
      supportHoldsForUnit.map(w => new PotentialSupportHoldOrder(v.id, w.id))
    })
  }
}