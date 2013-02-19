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
  type MovementST[A] = ST[MovementPhaseAdjudicator, 
    STRef[MovementPhaseAdjudicator, A]]
  
  def isAdjacentLocations(loc1: Int, loc2: Int): Boolean =
    DBQueries.adjacencies.exists(a => 
      a.srcLocation == loc1 && a.dstLocation == loc2
    )
    
  lazy val provinceWithParties: 
	  List[(Province, MovementST[List[DiplomacyUnit]])] = {
    val initialSet = DBQueries.provinces.map((prov: Province) =>
      (prov, newVar[MovementPhaseAdjudicator, List[DiplomacyUnit]](Nil))
    )
    
    val dpus = DBQueries.getDiplomacyUnitsForGameAtCurrentGameTime(game)
    dpus.map(dpu => {
      for (loc <- DBQueries.locations.find(_.id == dpu.unitLocationID);
    	   (prov, st) <-initialSet.find(_._1.id.compareTo(loc.province) == 0)
      ) yield {
        
      }
    })
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