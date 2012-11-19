package com.squeryl.jdip.creators
import com.squeryl.jdip.queries.DBQueries
import com.squeryl.jdip.tables._
import com.squeryl.jdip.functions._

class PotentialMoveOrderCreator(game: Game, dbQueries: DBQueries) {
  def getRegularMoves(diplomacyUnit: DiplomacyUnit): List[Location] =
    dbQueries.getAdjacentLocationsForLocation(
        dbQueries.getLocationFromDiplomacyUnit(diplomacyUnit))
    
  def getMovesByConvoy(diplomacyUnit: DiplomacyUnit): List[Location] = {
    diplomacyUnit match {
      case DiplomacyUnit(UnitType.ARMY, _, unitLocation, _, _) => 
        dbQueries.locations.filter(_.id == unitLocation).flatMap(loc => {
          val allPaths = 
            findAllPathsExternal(dbQueries,
                loc, dbQueries.getAllFleetUnitsForGame(game))
          allPaths.map(_ match {
            case h :: tail => h
          })
        })
      case _ => Nil
    }
  } 
  
  def getTotalMoves(diplomacyUnit: DiplomacyUnit): List[Location] = 
    getRegularMoves(diplomacyUnit) ++ getMovesByConvoy(diplomacyUnit)
  
  def createPotentialMoveOrders(): List[PotentialMoveOrder] = {
    val dpusForGame = 
      dbQueries.getDiplomacyUnitsForGameAtCurrentGameTime(game)
    
    dpusForGame.foldLeft(Nil: List[PotentialMoveOrder])((u, v) => {
      val moveLocations = getTotalMoves(v)
      val potentialMoveOrders =
        moveLocations.map(w => PotentialMoveOrder(v.id, w.id))
      u ++ potentialMoveOrders
    })
  }
}