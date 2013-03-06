package com.squeryl.jdip.adjudicators
import com.squeryl.jdip.queries.DBQueries
import com.squeryl.jdip.tables._
import scalaz.effects._
import scalaz.Forall
import scala.collection.immutable.TreeSet

object MovementPhaseAdjudicator {
  type Marker = Int
  val UNDETERRED_FLAG: Marker = 0
  val QUESTIONABLE_FLAG: Marker = 1
  val DISRUPTED_FLAG: Marker = 2
  val DISLOGDED_FLAG: Marker = 3
  val BOUNCE_FLAG: Marker = 4
}

class MovementPhaseAdjudicator(game: Game) {
  private type DiplomacyUnitOrderState = 
    (DiplomacyUnit, String, IORef[Marker])
  private type ProvinceWithParties = 
    (Province, IORef[List[DiplomacyUnitOrderState]])
  
  
  implicit object DPUOrdering extends Ordering[DiplomacyUnitOrderState] {
    
    override def compare(x: DiplomacyUnitOrderState, 
        y: DiplomacyUnitOrderState): Int =
      if (x._1.id < y._1.id) {
        -1
      } else if (x._1.id == y._1.id) {
        0
      } else {
        1
      } 
  }
  
  implicit object ProvinceOrdering extends Ordering[ProvinceWithParties] {
    override def compare(x: ProvinceWithParties, 
        y: ProvinceWithParties): Int =
          x._1.id.compareTo(y._1.id)
  }
  
  implicit def useLocationAsKey(loc: Location): ProvinceWithParties =
    (Province(loc.province, ""), newIORef[List[DiplomacyUnitOrderState]](Nil))
  private def hasMoveOrder(dpu: DiplomacyUnit) = 
    moveOrders.exists(_.id == dpu.id)
    
    
  private lazy val dpus =
    DBQueries.getDiplomacyUnitsForGameAtCurrentGameTime(game)
  private lazy val allOrdersForGame =
    DBQueries.getOrdersForDiplomacyUnits(dpus)
  private lazy val supportHoldOrders =
    allOrdersForGame.filter(_.orderType.compareTo(OrderType.SUPPORT_HOLD) == 0)
  private lazy val supportMoveOrders =
    allOrdersForGame.filter(_.orderType.compareTo(OrderType.SUPPORT_MOVE) == 0)
  private lazy val convoyOrders =
    allOrdersForGame.filter(_.orderType.compareTo(OrderType.CONVOY) == 0)
  private lazy val moveOrders =
    allOrdersForGame.filter(_.orderType.compareTo(OrderType.MOVE) == 0)
    
  private val diplomacyUnitOrderMap: TreeSet[DiplomacyUnitOrderState] = {
    val initialTreeSet = 
      new TreeSet[DiplomacyUnitOrderState]()
    dpus.foldLeft(initialTreeSet)((treeSet, dpu) => {
      val orderForDpu = allOrdersForGame.find(_.id == dpu.id) match {
        case Some(o: Order) => o.orderType
        case None => OrderType.HOLD
      }
      
      treeSet + ((dpu, 
          orderForDpu, 
          newIORef(UNDETERRED_FLAG).unsafePerformIO))
    })
  }
    
  def isAdjacentLocations(loc1: Int, loc2: Int): Boolean =
    DBQueries.adjacencies.exists(a => 
      a.srcLocation == loc1 && a.dstLocation == loc2
    )
    
  // This maps over the hold locations of the units.  A unit moving
    // to another province would not be considered as part of a hold
  private def populateSetWithDiplomacyUnits(
      provinceSet: TreeSet[ProvinceWithParties]): Unit = {
    
    dpus.foldLeft(provinceSet)((treeSet, dpu) => {
      for (loc <- DBQueries.locations.find(_.id == dpu.unitLocationID);
    	  (prov, ioRef) <- treeSet.from(loc).headOption
      ) yield {
    	  if (prov.id.compareTo(loc.province) == 0 && !hasMoveOrder(dpu)) {
    	    ioRef.write((dpu, 
    	        OrderType.HOLD, 
    	        newIORef[Marker](UNDETERRED_FLAG))).unsafePerformIO
    	  }
      }
      
      treeSet
    })
  }
  
  private def populateSetWithMovingDiplomacyUnits(
      partySet: ProvincesWithParties): Unit = {
    
    
  }
    
  
  private lazy val provinceWithParties: TreeSet[ProvinceWithParties] = {
    val initialTreeSet = 
      DBQueries.provinces.
      	foldLeft(new TreeSet[ProvinceWithParties])((treeSet, prov) => {
      	  treeSet + (
      	      (prov, 
      	      newIORef[List[DiplomacyUnitOrderState]](Nil).unsafePerformIO))
      	})
    
    
    populateSetWithDiplomacyUnits(initialTreeSet)
    populateSetWithMovingDiplomacyUnits(initialSet)
    
    initialSet
  }
  
  def getInitialPartyStrength: List[(Province, IORef[PartyStrength])] = {
    dpus.foreach(dpu => {
      for (loc <- DBQueries.locations.find(_.id == dpu.unitLocationID);
    		  (prev, ioRef) <- 
      )
    })
  }
  
  def getFinalPartyStrengths(
      currentPartyStrengths: List[(Province, IORef[PartyStrength])]): 
    	  List[(Province, PartyStrength)] = {
    
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