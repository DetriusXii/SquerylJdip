package com.squeryl.jdip.creators
import com.squeryl.jdip.queries.DBQueries
import com.squeryl.jdip.tables._
import com.squeryl.jdip.functions._

class PotentialConvoyOrderCreator(game: Game, dbQueries: DBQueries) {
  def createPotentialConvoyOrders(): List[PotentialConvoyOrder] = {
	val dpusForGame = dbQueries.getDiplomacyUnitsForGameAtCurrentGameTime(game)
	
	dpusForGame.foldLeft(Nil: List[PotentialConvoyOrder])((u, v) => {
	  
	  u ++ getConvoys(v, dpusForGame).foldLeft(
	      Nil: List[PotentialConvoyOrder])((w, x) => {
	    	val convoyUnitLocation = x._1
	    	val convoyUnitDestinations = x._2
	    	
	    	w ++ convoyUnitDestinations.map(y => 
	    	  new PotentialConvoyOrder(v.id, y.id, convoyUnitLocation.id))
	      })
	})
  }
  
  def getConvoys(diplomacyUnit: DiplomacyUnit, 
      allDiplomacyUnitsForGame: List[DiplomacyUnit]): 
	  List[(Location, List[Location])] = {
    diplomacyUnit match {
      case DiplomacyUnit(UnitType.FLEET, _, unitLocation, _, _) => {
        dbQueries.locations.find(_.id == unitLocation).flatMap(loc => {
          val isCoastal = dbQueries.hasLandLocation(loc)

          if (isCoastal) {
            None
          } else {
            Some(loc)
          }
        }).map(fleetLoc => {
          val allLandUnits: List[DiplomacyUnit] = 
            allDiplomacyUnitsForGame.filter(_.unitType.equals(UnitType.ARMY))
          val allFleetUnits: List[DiplomacyUnit] = 
            allDiplomacyUnitsForGame.filter(_.unitType.equals(UnitType.FLEET))
          val allLandUnitLocations = allLandUnits.
            map((ldpu: DiplomacyUnit) => dbQueries.
                locations.find(_.id == ldpu.unitLocation)).
                flatten
          allLandUnitLocations.map((loc: Location) => {
            val allPaths: List[List[Location]] = 
              findAllPathsExternal(dbQueries,loc, allFleetUnits)
            val targetDestinations: List[Location] = 
              allPaths.filter((path: List[Location]) => {
                path.exists(_.id == fleetLoc.id)
              }).map(_ match {
                case h :: tail => h
              })
              (loc, targetDestinations)
            }).filter(!_._2.isEmpty)
        }) match {
          case Some(x: List[(Location, List[Location])]) => x
          case None => Nil
        }
      }
      case _ => Nil
    }
  }
}