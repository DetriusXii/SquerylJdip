package com.squeryl.jdip.adjudicators
import com.squeryl.jdip.queries.DBQueries
import com.squeryl.jdip.tables._
import scalaz.effects.ST
import scalaz.effects.STRef
import scalaz.effects._
import scalaz.Forall

object MovementPhaseAdjudicator {
  val UNDETERRED_FLAG = 0
  val QUESTIONABLE_FLAG = 1
  val DISRUPTED_FLAG = 2
  val DISLOGDED_FLAG = 3
}

class MovementPhaseAdjudicator(game: Game) {
  type ProvincesWithParties = List[(Province, IORef[List[DiplomacyUnit]])] 
  
  
  def isAdjacentLocations(loc1: Int, loc2: Int): Boolean =
    DBQueries.adjacencies.exists(a => 
      a.srcLocation == loc1 && a.dstLocation == loc2
    )
    
  // This maps over the hold locations of the units.  A unit moving
    // to another province would still be considered as part of a hold
  private def populateSetWithDiplomacyUnits(
      partySet: ProvincesWithParties): Unit = {
    
    dpus.foreach(dpu => {
      for (loc <- DBQueries.locations.find(_.id == dpu.unitLocationID);
    	   (prov, ioRef) <- partySet.find(_._1.id.compareTo(loc.province) == 0)
      ) yield {
        ioRef.write(dpu :: Nil).unsafePerformIO
      }
    })
  }
  
  private def populateSetWithMovingDiplomacyUnits(
      partySet: ProvincesWithParties): Unit = {
    
    dpusWithMoveOrder.foreach(dpu => {
      for (loc <- DBQueries.locations.find(_.id == dpu.unitLocationID);
    	(prov, ioRef) <- partySet.find(_._1.id.compareTo(loc.province) == 0)
      ) yield {
        ioRef.write()
      }
    })
  }
    
  
  lazy val provinceWithParties: ProvincesWithParties = {
    val initialSet = DBQueries.provinces.map((prov: Province) => {
      (prov, newIORef[List[DiplomacyUnit]](Nil).unsafePerformIO)
    })
    populateSetWithDiplomacyUnits(initialSet)
    populateSetWithMovingDiplomacyUnits(initialSet)
    
    initialSet
  }
  
  lazy val dpusWithMoveOrder = 
    DBQueries.getDiplomacyUnitsWithMoveOrderForGameAtCurrentTime(game)
  lazy val dpus =
    DBQueries.getDiplomacyUnitsForGameAtCurrentGameTime(game)
  
  lazy val provincePartyStrength: List[Province, List]
    
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