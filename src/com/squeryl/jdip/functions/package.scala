package com.squeryl.jdip

import com.squeryl.jdip.tables._
import com.squeryl.jdip.queries.DBQueries

package object functions {
	def findAllPathsExternal(dbQueries: DBQueries,
	    originLocation: Location, 
	    allFleetUnits: List[DiplomacyUnit]): List[List[Location]] = {
    def findAllPaths(currentLocation: Location,
    	allPaths: List[List[Location]],
    	presentPath: List[Location]): List[List[Location]] = {
      val isCurrentLocOnOriginLoc = currentLocation match {
        case Location(originLocation.province, originLocation.coast) => true
        case _ => false
      }
      val isCurrentLocOnPath = presentPath.exists(_.id == currentLocation.id)
      val hasFleetUnit = allFleetUnits.exists(_.unitLocation == currentLocation.id)
      val isCurrentLocLandLocation = currentLocation match {
        case Location(_, Coast.NO_COAST) => true
        case _ => false
      }
     
      val isCurrentLocOnOriginProvince = 
        currentLocation.province.equals(originLocation.province)
      val hasLandLocation = dbQueries.hasLandLocation(currentLocation)
      
      if (isCurrentLocLandLocation && isCurrentLocOnOriginLoc) {
        val newPath = currentLocation :: presentPath
        val coastLocations = 
          dbQueries.getCoastLocationsFromLandLocation(currentLocation)
        coastLocations.foldLeft(allPaths)((u, v) => {
          findAllPaths(v, allPaths, newPath)
        })
      } else if (isCurrentLocLandLocation && !isCurrentLocOnOriginLoc) {
        val newPath = currentLocation :: presentPath
        newPath :: allPaths
      } else if (!isCurrentLocLandLocation && isCurrentLocOnOriginProvince) {
        val adjLocations = 
          dbQueries.getAdjacentLocationsForLocation(currentLocation)
        val oceanAdjLocations = 
          adjLocations.filter(!dbQueries.hasLandLocation(_))
        val newPath = currentLocation :: presentPath
        oceanAdjLocations.foldLeft(allPaths)((u, v) => findAllPaths(v, u, newPath))
      } else if (!isCurrentLocOnOriginProvince && hasLandLocation) {
        val newPath = currentLocation :: presentPath;
        val landLocationOption = dbQueries.locations.find(_ match {
          case Location(currentLocation.province, Coast.NO_COAST) => true
          case _ => false
        })
        landLocationOption match {
          case Some(loc: Location) => findAllPaths(loc, allPaths, newPath)
          case None => allPaths
        }
      } else if (isCurrentLocOnPath) {
        allPaths
      } else if (!isCurrentLocOnPath && hasFleetUnit) {
        val newPath = currentLocation :: presentPath
        val adjLocations = 
          dbQueries.getAdjacentLocationsForLocation(currentLocation)
        adjLocations.foldLeft(allPaths)((u, v) => findAllPaths(v, u, newPath))
      } else {
        allPaths
      }
    }
  
    findAllPaths(originLocation, Nil, Nil)
  }
}