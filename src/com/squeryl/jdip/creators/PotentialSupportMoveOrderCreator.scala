package com.squeryl.jdip.creators
import com.squeryl.jdip.tables._
import com.squeryl.jdip.queries.DBQueries

class PotentialSupportMoveOrderCreator(game: Game, dbQueries: DBQueries) {
  def getSupportMoves(diplomacyUnit: DiplomacyUnit, 
      allUnitsForGame: List[DiplomacyUnit]): 
	  List[(Location, List[Location])] = {
    val allOtherUnits = 
      allUnitsForGame.filter(_.id != diplomacyUnit.id)
     
    val potentialMoveOrderCreator = new PotentialMoveOrderCreator(game, dbQueries)
    
    val movesForAllOtherUnits: List[(Location, List[Location])] = 
      allOtherUnits.map(dpu => {
        val otherUnitLocation = dbQueries.locations.find(_.id == dpu.unitLocation)
        otherUnitLocation.map((_, potentialMoveOrderCreator.getTotalMoves(dpu)))
      }).flatten
    
    val regularMovesForThisUnit = 
      potentialMoveOrderCreator.getRegularMoves(diplomacyUnit)
    
    val movesThatMatterForAllOtherUnits = movesForAllOtherUnits.map(u => {
      val otherUnitLocation = u._1
      val totalMovesForOtherUnits = u._2
      
      val tMovesForOtherUnitsWhereThisUnitCanReach =
        totalMovesForOtherUnits.filter(loc => regularMovesForThisUnit.exists(_ match {
          case Location(loc.province, _) => true
          case _ => false
        }))
        
      (otherUnitLocation, tMovesForOtherUnitsWhereThisUnitCanReach)
    })
    
    val filteredMovesThatMatterForAllOtherUnits = 
      movesThatMatterForAllOtherUnits.filter(!_._2.isEmpty)
    filteredMovesThatMatterForAllOtherUnits
  } 
  
  
  def createPotentialSupportMoveOrders(): List[PotentialSupportMoveOrder] = {
    val dpusForGame = dbQueries.getDiplomacyUnitsForGameAtCurrentGameTime(game)
    
    dpusForGame.foldLeft(Nil: List[PotentialSupportMoveOrder])((u, v) => {
      val supportMovesForGame = getSupportMoves(v, dpusForGame)
      
      u ++ supportMovesForGame.foldLeft(
          Nil: List[PotentialSupportMoveOrder])((w, y) => {
        val movingUnitLocation = y._1
        val movingUnitDestinationLocations = y._2
        
        val potentialSupportMoveOrders =
          movingUnitDestinationLocations.map(x => 
          	new PotentialSupportMoveOrder(v.id, x.id, movingUnitLocation.id))
        w ++ potentialSupportMoveOrders
      })
    })
  }
}