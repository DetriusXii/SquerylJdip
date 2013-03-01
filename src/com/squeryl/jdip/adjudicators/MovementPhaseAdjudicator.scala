package com.squeryl.jdip.adjudicators
import com.squeryl.jdip.queries.DBQueries
import com.squeryl.jdip.tables._
import scalaz.effects.ST
import scalaz.effects.STRef
import scalaz.effects._

object MovementPhaseAdjudicator {
  val UNDETERRED_FLAG = 0
  val QUESTIONABLE_FLAG = 1
  val DISRUPTED_FLAG = 2
  val DISLOGDED_FLAG = 3
}

class MovementPhaseAdjudicator(game: Game) {
  type STRefST[S, A] = ST[S, STRef[S, A]]
  type PartiesInProvinceType[S] = 
    List[(Province, STRefST[S, List[DiplomacyUnit]])]
  
  def isAdjacentLocations(loc1: Int, loc2: Int): Boolean =
    DBQueries.adjacencies.exists(a => 
      a.srcLocation == loc1 && a.dstLocation == loc2
    )
    
  // This maps over the hold locations of the units.  A unit moving
    // to another province would still be considered as part of a hold
  private def populateSetWithDiplomacyUnits(
      partySet: PartiesInProvinceType): Unit = {
    val dpus = DBQueries.getDiplomacyUnitsForGameAtCurrentGameTime(game)
    
    dpus.foreach(dpu => {
      for (loc <- DBQueries.locations.find(_.id == dpu.unitLocationID);
    	   (prov, st) <- partySet.find(_._1.id.compareTo(loc.province) == 0)
      ) yield {
    	for ( stRef <- st;
    	    _ <- stRef.write(dpu :: Nil)
    	) yield {
    	  ()
    	}  
      }
    })
  }
  
  private def populateSetWithMovingDiplomacyUnits(
      partySet: PartiesInProvinceType): Unit = {
    
    val dpusWithMoveOrder =
      DBQueries.getDiplomacyUnitsWithMoveOrderForGameAtCurrentTime(game)
    
    dpusWithMoveOrder.foreach(dpu => {
      for (loc <- DBQueries.locations.find(_.id == dpu.unitLocationID);
    		  (prov, st) <- partySet.find(_._1.id.compareTo(loc.province) == 0)
      ) yield {
        for (stRef <- st;
        	curList <- stRef.read;
        	_ <- stRef.write(dpu :: curList)
        ) yield {
          ()
        }
      }
    })
  }
    
  lazy val provinceWithParties: 
	  List[(Province, MovementST[List[DiplomacyUnit]])] = {
    val initialSet = DBQueries.provinces.map((prov: Province) =>
      (prov, newVar[MovementPhaseAdjudicator, List[DiplomacyUnit]](Nil))
    )
    
    populateSetWithDiplomacyUnits(initialSet)
    populateSetWithMovingDiplomacyUnits(initialSet)
    
    
    initialSet
  }
  
  def provinceWithParties[S]: PartiesInProvinceType[S] = {
    val initialSet = DBQueries.provinces.map((prov: Province))
  }
  
    
	def adjudicateGame: Unit = {
	  val diplomacyUnitsForGame =
	    DBQueries.getDiplomacyUnitsForGameAtCurrentGameTime(game)
	    
	  val dpuOrderTuples = diplomacyUnitsForGame.map((dpu: DiplomacyUnit) => {
	    val orderForDpu = DBQueries.getOrderForDiplomacyUnit(dpu)
	    (dpu, orderForDpu) 
	  })
	    
	  val moveOrderTuples = dpuOrderTuples.filter(dpuOrderTuple =>
	    dpuOrderTuple._2 match {
	      case Some(o: Order) => o.orderType.compareTo(OrderType.MOVE) == 0
	      case None => false
	    } 
	  ).map(dpuOrderTuple => dpuOrderTuple._2.map((dpuOrderTuple._1, _))).
	  flatten
	  
	  val moveOrdersConvoySupported = moveOrderTuples.filter(dpuOrderTuple => {
	    val dpu = dpuOrderTuple._1
	    val order = dpuOrderTuple._2
	    
	    val isAdjacent = order.srcLocationIDOption match {
	      case Some(tgtLocation: Int) => 
	        isAdjacentLocations(dpu.unitLocationID, tgtLocation)
	    }
	    
	    if (isAdjacent) {
	      retu
	    }
	  })
	}
}