package com.squeryl.jdip

import scalaz.effect.IO
import com.squeryl.jdip.tables.Order
import com.squeryl.jdip.tables.OrderType
import com.squeryl.jdip.queries.DBQueries
import scalaz._
import scalaz.std._
import scalaz.syntax._
import scalaz.std.list.listInstance._
import com.squeryl.jdip.tables.Province

package object adjudicators {
  def cutSupport(moveOS: OrderState)
  	(isStepNineOfAlgorithm: Boolean)
  	(isMoveByConvoy: Boolean)
  	(fleetUnitsOnPath:  List[List[OrderState]])
  	(supports: List[OrderState]): IO[Unit] = {
	  	  
	  	def findPathsNotInvolvingSupportUnit(
	  	    supportedFleetUnitLocationID: Int): List[List[OrderState]] = 
	  			fleetUnitsOnPath.filter(
	  			    _.exists(os => 
	  			      os.getPresentOrder.map(o => 
	  			        o.orderType.compareTo(OrderType.HOLD) != 0 && 
	  			        os.dpu.unitLocationID == supportedFleetUnitLocationID).unsafePerformIO
	  			))
	  	def getSupportedUnit(supportOS: OrderState): Option[Int] = 
	  	  if (supportOS.order.orderType.compareTo(OrderType.SUPPORT_HOLD) == 0)
	  	    supportOS.order.dstLocationIDOption
	  	  else
	  	    supportOS.order.srcLocationIDOption      
	  			
	  	def findSupportOS: Option[OrderState] = 
	  	  for (moveDSTLocationID <- moveOS.order.dstLocationIDOption;
	  	    supportOS <- supports.find(_.dpu.unitLocationID == moveDSTLocationID);
	  	    _ <- Some(supportOS) if 
	  	    	!isSupportOwnedBySamePowerAsMovingUnit(supportOS)
	  	  ) yield supportOS
	  	  
	  	def isSupportCut(supportOSPresentOrder: Order) = 
	  	  supportOSPresentOrder.orderType.compareTo(OrderType.HOLD) == 0
	  	def isSupportOwnedBySamePowerAsMovingUnit(supportOS: OrderState) =
	  	  supportOS.dpu.gamePlayerEmpireID == moveOS.dpu.gamePlayerEmpireID
	  	def isSupportingUnitSupportingAllPaths(supportedFleetUnitLocationID: Int) = 
	  	  findPathsNotInvolvingSupportUnit(supportedFleetUnitLocationID).size == 0
	  	def isSupportingUnitOfferingSupportToMoveSpace(supportedUnitLocationID: Int) = 
	  	  moveOS.order.dstLocationIDOption.map(_ == supportedUnitLocationID).getOrElse({false})
	  	  
	  	  
	  	def handleCutSupportHelper(supportOS: OrderState, supportOSPresentOrder: Order): IO[Unit] = 
	  	  (for (_ <- Some() if !isSupportCut(supportOSPresentOrder);
  			  supportedUnitLocationID <- getSupportedUnit(supportOS);
  			  _ <- Some() if !isSupportingUnitSupportingAllPaths(supportedUnitLocationID);
  			  _ <- Some() if isStepNineOfAlgorithm || !isSupportingUnitOfferingSupportToMoveSpace(supportedUnitLocationID)
	  	  ) yield (moveOS.setPresentOrderToHold)).getOrElse({IO()})
	  	      
	  	  
	  	moveOS.getPresentOrder().flatMap(_.orderType match {
	  	  case OrderType.HOLD => IO()
	  	  case OrderType.MOVE => findSupportOS.map(supportOS => 
	  	    supportOS.getPresentOrder.flatMap(presentOrder => 
	  	    	handleCutSupportHelper(supportOS, presentOrder))).getOrElse({IO()})
	  	})
  	}
  
    def checkDisruptions(worldState: WorldState)
  	(moveByConvoyOrderState: OrderState)
  	(fleetUnitsOnPaths: List[List[OrderState]]): IO[Unit] = {
    def getProvinceIDFromLocationID(locationID: Int): Option[String] = 
      DBQueries.locations.find(_.id == locationID).map(_.province) 
    def getCombatListOrderStates(orderState: OrderState): ListT[IO, OrderState] = 
      getProvinceIDFromLocationID(orderState.dpu.unitLocationID).map(provinceID =>
          ListT.fromList(worldState.getCombatListForProvince(provinceID))).
          getOrElse({ListT.empty[IO, OrderState](IO.ioMonad)})
      
    val filteredListOfPaths = fleetUnitsOnPaths.filter(fleetUnitsOnPath =>
    	!fleetUnitsOnPath.exists(fleetUnit => {
    		val fleetUnitSupportCount = fleetUnit.getSupportCount().unsafePerformIO()
    		val otherCombatListIO = DBQueries.locations.find(_.id == fleetUnit.dpu.unitLocationID).map(loc =>
    		    worldState.getCombatListForProvince(loc.province).
    		    map(_.filter(_.dpu.unitLocationID != fleetUnit.dpu.unitLocationID))).getOrElse({IO(Nil)})
		    val combatListWithSupportCount =
		      otherCombatListIO.flatMap(cl => sequence(cl.map(os => os.getSupportCount().map((_, os)))))
		    combatListWithSupportCount.map(_.find(supportCountOrderState => 
		      supportCountOrderState._1 > fleetUnitSupportCount).isEmpty).unsafePerformIO()
    	})
    )
    
    filteredListOfPaths match {
      case Nil => moveByConvoyOrderState.setMark(OrderState.CONVOY_ENDANGERED)
      case _ => IO()
    }
  }
    
    def bounce(worldState: WorldState)
    	(movingUnit: OrderState): IO[WorldState] = {
	  def getProvince : Option[Province] = 
	    for (l <- DBQueries.locations.find(_.id == movingUnit.dpu.unitLocationID);
	    		p <- DBQueries.provinces.find(_.id.compareTo(l.province) == 0)
	    ) yield p
	    
	  def addCombatUnitToProvince(p: Province): IO[WorldState] = 
	  	for (_ <- movingUnit.setMark(OrderState.BOUNCE);
			  _ <- movingUnit.setSupportCountToZero();
			  _ <- movingUnit.setNoHelpListToEmpty();
			  _ <- worldState.addCombatUnitToProvince(p, movingUnit)
    	  ) yield worldState
    	    
      getProvince.map(addCombatUnitToProvince).getOrElse(IO({worldState}))
    }
    
    def unbounce(worldState: WorldState)(p: Province): IO[Unit] = { 
      def getOrderStateAndMark(os: OrderState): IO[(Int, OrderState)] =
        os.getSupportCount().map((_, os))
      
      def getSequencedCombatList(combatList: List[OrderState]): IO[List[(Int, OrderState)]] = 
        sequence(combatList.map(getOrderStateAndMark))
      
      def findOrderStatesWithMaxSupportCount(sequencedCombatList: List[(Int, OrderState)]): List[(Int, OrderState)] =
        sequencedCombatList.foldLeft[List[(Int, OrderState)]](Nil)((maxOrderStates, os) => maxOrderStates match {
          case Nil => os :: Nil
          case h :: _ if h._1 < os._1 => os :: Nil
          case h :: _ if h._1 > os._1 => maxOrderStates
          case _ => os :: maxOrderStates
        })
        
      def getHeadOfListIfLength1(l : List[(Int, OrderState)]): Option[OrderState] = l.length match {
        case 1 => l.headOption.map(_._2)
        case _ => None
      }
      
      def getUnitProvince(os: OrderState): Option[Province] =
        for (l <- DBQueries.locations.find(_.id == os.dpu.unitLocationID);
        	p <- DBQueries.provinces.find(_.id.compareTo(l.province) == 0)
        ) yield p
      
      def removeUnitFromCombatList(os: OrderState)(p: Province): IO[Unit] 
        
      def removeMarksFromOrderState(os: OrderState): IO[Unit] = 
        for (mark <- os.getMark();
        		mark match {
        		  case OrderState.BOUNCE || OrderState.DISLODGED => os.setMark(OrderState.NO_MARK)
        		  case _ => 
        		}
        )
      
      for (combatList <- worldState.getCombatListForProvince(p.id);
          sequencedCombatList <- getSequencedCombatList(combatList);
          orderStateOption <- IO({getHeadOfListIfLength1(findOrderStatesWithMaxSupportCount(sequencedCombatList))})
      ) yield orderStateOption
      
      
    }
}