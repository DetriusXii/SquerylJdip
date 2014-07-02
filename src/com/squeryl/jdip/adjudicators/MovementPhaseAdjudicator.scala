package com.squeryl.jdip.adjudicators
import com.squeryl.jdip.queries.DBQueries
import com.squeryl.jdip.tables._
import scalaz.effect.IO._
import scalaz.effect._
import scalaz.OptionT._
import scala.collection.immutable.TreeSet
import java.sql.Timestamp
import scalaz.concurrent.Promise
import scalaz.{Order => _, _}
import scalaz.OptionT._
import scalaz.iteratee._
import scalaz.std.list._
import com.squeryl.jdip.functions._

object MovementPhaseAdjudicator {
  type Marker = Int
  val UNDETERRED_FLAG: Marker = 0
  val QUESTIONABLE_FLAG: Marker = 1
  val DISRUPTED_FLAG: Marker = 2
  val DISLOGDED_FLAG: Marker = 3
  val BOUNCE_FLAG: Marker = 4
}

object OrderState {
  type RESOLUTION_STATE = Int
  val UNRESOLVED: RESOLUTION_STATE = 0
  val GUESSING: RESOLUTION_STATE = 1
  val RESOLVED: RESOLUTION_STATE = 2

  type MARK = String
  val NO_MARK: MARK = ""
  val CONVOY_ENDANGERED: MARK = "convoy endangered"
  val CUT: MARK = "cut"
  val VOID: MARK = "void"
  val CONVOY_UNDER_ATTACK: MARK = "convoy under attack"
}

class OrderState(val dpu: DiplomacyUnit, val order: Order) {
   private val orderState: IO[IORef[OrderState.RESOLUTION_STATE]] = newIORef({OrderState.UNRESOLVED})
   private val supportCount: IO[IORef[Int]] = newIORef({0})
   private val helpList: IO[IORef[List[DiplomacyUnit]]] = newIORef({Nil})
   private val presentOrder: IO[IORef[Order]] = newIORef({order})
   private val noHelpList: IO[IORef[List[DiplomacyUnit]]] = newIORef({Nil})
   private val mark: IO[IORef[OrderState.MARK]] = newIORef({OrderState.NO_MARK})
   
   def setPresentOrderToHold(): IO[Unit] = 
     presentOrder.flatMap(_.write(Order(dpu.id, OrderType.HOLD, new Timestamp(0L), None, None)))
   def getPresentOrder(): IO[Order] = presentOrder.flatMap(_.read)
   def increaseSupportCount(supportingDpu: DiplomacyUnit): IO[Int] =
     for (	scIORef <- supportCount; 
		 	hlIORef <- helpList; 
		 	_ <- hlIORef.mod(supportingDpu :: _); 
		 	newSCCount <- scIORef.mod(_ + 1)
     ) yield newSCCount
   def addNoHelpingUnit(supportingDiplomacyUnit: DiplomacyUnit): IO[Unit] =
     noHelpList.flatMap(_.mod(supportingDiplomacyUnit :: _)).map(_ => ())
   def setMark(newMark: OrderState.MARK): IO[Unit] = mark.flatMap(markIORef =>
     markIORef.write({newMark})
   )
   def getMark(): IO[OrderState.MARK] = mark.flatMap(_.read)
   def getSupportCount(): IO[Int] = supportCount.flatMap(_.read)
}

class WorldState(provinces: List[Province]) {
  private val combatList: List[(Province, IO[IORef[List[OrderState]]])] = 
	  provinces.map((_, newIORef[List[OrderState]]({Nil})))
  
  def addCombatUnitToProvince(province: Province, combatUnit: OrderState): IO[Unit] = 
    combatList.find(_._1.id == province.id).map(t =>
    	for ( ioRef <- t._2;
			_ <- ioRef.mod(combatUnit :: _)
    	) yield (())
    ).getOrElse(IO(()))
    
  def getCombatListForProvince(provinceID: String): IO[List[OrderState]] = 
    combatList.find(_._1.id.compareTo(provinceID) == 0).map(_._2.flatMap(_.read)).getOrElse(IO({Nil}))
}


class MovementPhaseAdjudicator(game: Game) {
  private def createHoldOrder(dpu: DiplomacyUnit): Order = Order(dpu.id, OrderType.HOLD, new Timestamp(0L), None, None)  
  
  private def areConvoyOrdersAssistingMovingUnit(
      movingUnit: OrderState, 
      convoyOrders: List[OrderState], 
      singlePath: List[Location]): Boolean = {
    // remove both the source destination and the end landpoint from the list
    val unitLocationRemoved = singlePath.tail.reverse
    val targetLocation :: waterLocationsOnly = unitLocationRemoved
    
    val relevantConvoyOrders = convoyOrders.filter(os => 
      (for (srcLocationID <- os.order.srcLocationIDOption;
    		  dstLocationID <- os.order.dstLocationIDOption
      ) yield (
          movingUnit.dpu.unitLocationID == srcLocationID &&
          targetLocation.id == dstLocationID &&
          waterLocationsOnly.exists(_.id == os.dpu.unitLocationID)
      )).getOrElse({false})   
    )
   
    relevantConvoyOrders.length == waterLocationsOnly
  }
  
  private def hasConvoyPath(movingUnit: OrderState, 
      convoyOrders: List[OrderState], allFleetUnits: List[DiplomacyUnit]): Boolean = {
    
    
    val movingUnitLocationOption = DBQueries.locations.find(_.id == movingUnit.dpu.unitLocationID)
    val allPossibleConvoyPathsOption = movingUnitLocationOption.map(
        com.squeryl.jdip.functions.findAllPathsExternal(_, allFleetUnits))
    
    (for (allPossibleConvoyPaths <- allPossibleConvoyPathsOption
    ) yield (allPossibleConvoyPaths.exists(
        areConvoyOrdersAssistingMovingUnit(movingUnit, convoyOrders, _)))).getOrElse({false})
    
  }
  
  private def isSupportHoldOrderDefendingUnit(sho: OrderState, 
		  holdOrders: List[OrderState]): Boolean = 
    sho.order.dstLocationIDOption.map(dstLocationID =>
        holdOrders.exists(_.dpu.unitLocationID == dstLocationID)).getOrElse({false})
 
  private def getSupportHoldOrderDefendingUnit(sho: OrderState,
		  holdOrders: List[OrderState]): Option[OrderState] =
    for (dstLocationID <- sho.order.dstLocationIDOption;
		holdOrder <- holdOrders.find(_.dpu.unitLocationID == dstLocationID)
    ) yield holdOrder
      
  private def getSupportMoveOrderSupportingUnit(smo: OrderState,
      moveOrders: List[OrderState]): Option[OrderState] = 
	  	for (dstLocationID <- smo.order.dstLocationIDOption;
				  srcLocationID <- smo.order.srcLocationIDOption;
				  dpuOrderState <- moveOrders.find(_.dpu.unitLocationID == srcLocationID)
		 ) yield dpuOrderState
    
  private def isSupportMoveOrderSupportingUnit(smo: OrderState,
		  moveOrders: List[OrderState]): Boolean =
		  (for (dstLocationID <- smo.order.dstLocationIDOption;
				  srcLocationID <- smo.order.srcLocationIDOption;
				  dpuOrderState <- moveOrders.find(_.dpu.unitLocationID == srcLocationID);
				  targetLocationID <- dpuOrderState.order.dstLocationIDOption
		  ) yield (targetLocationID == dstLocationID)).getOrElse({false})
  
  
  private def doesConvoyOrderAgreeWithUnitOrder(co: OrderState, 
      moveOrdersByConvoy: List[OrderState]): Boolean =
    (for (srcLocationID <- co.order.srcLocationIDOption;
	  		dstLocationID <- co.order.dstLocationIDOption;
	  		dpu <- moveOrdersByConvoy.find(_.dpu.unitLocationID == srcLocationID);
	  		unitTargetLocationID <- dpu.order.dstLocationIDOption 
  	) yield (dstLocationID == unitTargetLocationID)).getOrElse({false})
 
  private def increaseSupportCountOfHoldOrder(holdOrders: List[OrderState])(sho: OrderState): 
	  IO[Unit] = 
	    (for (dstLocationID <- sho.order.dstLocationIDOption;
	        holdOS <- holdOrders.find(_.dpu.unitLocationID == dstLocationID))
	    yield (holdOS.increaseSupportCount(sho.dpu).map(_ => ()))).getOrElse(sho.setPresentOrderToHold)
  
  private def increaseSupportCountOfMoveOrder(moveOrders: List[OrderState])(smo: OrderState):
	  IO[Unit] =
	    (for (targetLocationID <- smo.order.dstLocationIDOption;
	        srcLocationID <- smo.order.srcLocationIDOption;
	    	moveOS <- moveOrders.find(_.dpu.unitLocationID == srcLocationID);
	    	dstLocationID <- moveOS.order.dstLocationIDOption;
	    	_ <- Some(moveOS) if targetLocationID == srcLocationID
	    ) yield (moveOS.increaseSupportCount(smo.dpu).map(_ => ()))).getOrElse(smo.setPresentOrderToHold)
	
  private def increaseNoHelpList(moveOrders: List[OrderState], 
      allOrders: List[OrderState])(smo: OrderState): IO[Unit] =
    (for (targetLocationID <- smo.order.dstLocationIDOption;
    	srcLocationID <- smo.order.srcLocationIDOption;
    	moveOS <- moveOrders.find(_.dpu.unitLocationID == srcLocationID);
    	dstLocationID <- moveOS.order.dstLocationIDOption;
    	_ <- Some(moveOS) if targetLocationID == srcLocationID;
    	_ <- allOrders.find(otherOrder =>
    	  otherOrder.dpu.unitLocationID == targetLocationID && 
    	  otherOrder.dpu.gamePlayerEmpireID == smo.dpu.gamePlayerEmpireID)
    ) yield (moveOS.addNoHelpingUnit(smo.dpu))).getOrElse(IO(()))
  
  private def handleCutSupportHelper(supportOS: OrderState, moveOS: OrderState, 
      supportOSPresentOrder: Order): IO[Unit] = 
        (for (_ <- Some() if supportOSPresentOrder.orderType.compareTo(OrderType.HOLD) != 0
            
        ) yield IO()).getOrElse(IO())
  
  def makeEnumerator(xs: List[OrderState]): Enumerator[OrderState] = 
    Iteratee.enumList[OrderState, Id.Id](xs)
    
  def exists(predicate: OrderState => Boolean): Iteratee[OrderState, Boolean] = {
	def step(wasFound: Boolean): Input[OrderState] => Iteratee[OrderState, Boolean] = {
	  case Input.Element(orderState) if predicate(orderState) => Iteratee.done(true, Iteratee.eofInput[OrderState])
	  case Input.Element(orderState) => Iteratee.cont(step(false))
	  case Input.Eof() => Iteratee.done(false, Iteratee.eofInput[OrderState])
	  case Input.Empty() => Iteratee.done(false, Iteratee.eofInput[OrderState])
	} 
    Iteratee.cont(step(false))
  }
  
  
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
   
  private def mapConvoyPathsToFleetUnitOrderStates
  	(convoyPaths: List[List[Location]])
  	(orderStates: List[OrderState]): List[List[OrderState]] = 
    convoyPaths.map(_.map(l => orderStates.find(_.dpu.unitLocationID == l.id)).flatten)
  
  private def getMoveByConvoyOrders(moveOrders: List[OrderState]): List[OrderState] =
    moveOrders.filter(os => {
	    val unitLocationID = os.dpu.unitLocationID
	    val dstLocationIDOption = os.order.dstLocationIDOption
	    !dstLocationIDOption.map(dstLocationID => 
	      DBQueries.adjacencies.exists(adj => 
	        adj.srcLocation == unitLocationID &&
	        adj.dstLocation == dstLocationID  
	      )).getOrElse({false})
	  })
	  
  private def getRegularMoveOrders(moveOrders: List[OrderState]): List[OrderState] = 
    moveOrders.filter(os => {
	    val unitLocationID = os.dpu.unitLocationID
	    val dstLocationIDOption = os.order.dstLocationIDOption
	    dstLocationIDOption.map(dstLocationID =>
	    	DBQueries.adjacencies.exists(adj =>
	    		adj.srcLocation == unitLocationID &&
	    		adj.dstLocation == dstLocationID
	    	)
	    ).getOrElse({false})
	  })
  
  def evaluateMoveByConvoys
  	(moveByConvoyOrders: List[OrderState])
  	(convoyOrders: List[OrderState])
  	(fleetUnitsForGame: List[DiplomacyUnit]): Unit = moveByConvoyOrders.map(moveByConvoyOrder =>
	    Promise[IO[Unit]] {
	      if (!hasConvoyPath(moveByConvoyOrder, convoyOrders, fleetUnitsForGame)) {
	        moveByConvoyOrder.setPresentOrderToHold()
	      } else {
	        IO(())
	      }
	    }
	  ).foldLeft(IO(()))((u, v) => u.flatMap(_ => v.get)).unsafePerformIO
  
  private def evaluateConvoys
  	(convoyOrders: List[OrderState])
  	(moveOrders: List[OrderState]): Unit =
    	  convoyOrders.map(co => 
	  	Promise[IO[Unit]] {
	  	  if (doesConvoyOrderAgreeWithUnitOrder(co, moveOrders)) 
	  	    IO(())
	  	  else co.setPresentOrderToHold
	  	}
	  ).foldLeft(IO(()))((u, v) => u.flatMap(_ => v.get)).unsafePerformIO
	  
  private def getAllOrderStates(diplomacyUnits: List[DiplomacyUnit]): List[OrderState] =
	  diplomacyUnits.map((dpu: DiplomacyUnit) => {
	    val orderForDpu = DBQueries.getOrderForDiplomacyUnit(dpu).getOrElse(createHoldOrder(dpu))
	    new OrderState(dpu, orderForDpu)
	  })
	  
  private def addPresentOrderToCombatList
  	(worldState: WorldState)
  	(orderState: OrderState)
  	(presentOrder: Order): IO[Unit] = {
    def findProvinceByLocationID(locationID: Int): Option[Province] =
      for (location <- DBQueries.locations.find(_.id == locationID);
    		  province <- DBQueries.provinces.find(_.id == location.province)
      ) yield province
   
    def getProvinceForOrder: Option[Province] = presentOrder.orderType match {
        case OrderType.MOVE => presentOrder.dstLocationIDOption.flatMap(dstLocationID => 
      		findProvinceByLocationID(dstLocationID))
        case _ => findProvinceByLocationID(orderState.dpu.unitLocationID)
	
	  }
    
    getProvinceForOrder.map(worldState.addCombatUnitToProvince(_, orderState)).getOrElse({IO(())})
  }
  
  private def foldListOfIO[A](l : List[IO[A]]): IO[List[A]] = l.foldLeft(IO(List.empty[A]))((u, v) =>
  	for(foldedList <- u; element <- v) yield (element :: foldedList)
  )
  
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
		      otherCombatListIO.flatMap(cl => foldListOfIO(cl.map(os => os.getSupportCount().map((_, os)))))
		    combatListWithSupportCount.map(_.find(supportCountOrderState => 
		      supportCountOrderState._1 > fleetUnitSupportCount).isEmpty).unsafePerformIO()
    	})
    )
    
    filteredListOfPaths match {
      case Nil => moveByConvoyOrderState.setMark(OrderState.CONVOY_ENDANGERED)
      case _ => IO()
    }
  }
  
  def step4Execution(moveByConvoyOrders: List[OrderState], 
      worldState: WorldState, 
      fleetUnitsOnPath: List[List[OrderState]],
      supports: List[OrderState]): IO[Unit] = moveByConvoyOrders.foldLeft(IO(()))((u, v) =>
    for (_ <- checkDisruptions(worldState)(v)(fleetUnitsOnPath);
    		markForUnit <- v.getMark();
    		_ <- markForUnit match { 
    		  case OrderState.NO_MARK => cutSupport(v)(false)(true)(fleetUnitsOnPath)(supports)
    		  case _ => v.setMark(OrderState.CONVOY_UNDER_ATTACK)
    		}
  )
	  
  def adjudicateGame: IO[Unit] = {
	  val diplomacyUnitsForGame =
	    DBQueries.getDiplomacyUnitsForGameAtCurrentGameTime(game)
	  val fleetUnitsForGame = diplomacyUnitsForGame.filter(_.unitType.compareTo(UnitType.FLEET) == 0)  
	  
	  
	  val orderStates = getAllOrderStates(diplomacyUnitsForGame)
	  
	  val holdOrders = orderStates.filter(_.order.orderType.compareTo(OrderType.HOLD) == 0)
	  val moveOrders = orderStates.filter(_.order.orderType.compareTo(OrderType.MOVE) == 0)
	  val moveByConvoyOrders = getMoveByConvoyOrders(moveOrders)
	  val regularMoveOrders = getRegularMoveOrders(moveOrders)
	  
	  val convoyOrders = orderStates.filter(_.order.orderType.compareTo(OrderType.CONVOY) == 0)
	  evaluateMoveByConvoys(moveByConvoyOrders)(convoyOrders)(fleetUnitsForGame)   
	  evaluateConvoys(convoyOrders)(moveOrders)
	  
	  val supportHoldOrders = 
	    orderStates.filter(_.order.orderType.compareTo(OrderType.SUPPORT_HOLD) == 0)
	  val supportMoveOrders = 
	    orderStates.filter(_.order.orderType.compareTo(OrderType.SUPPORT_MOVE) == 0)
	  val supportOrders = supportHoldOrders ++ supportMoveOrders
	  
	  def holdOrderIncrease = increaseSupportCountOfHoldOrder(holdOrders) _
	  def moveOrderIncrease = increaseSupportCountOfMoveOrder(moveOrders) _
	  def noHelpIncrease = increaseNoHelpList(moveOrders, orderStates) _
	  
	  val currentExecution = supportHoldOrders.foldLeft(IO(()))(
	  	    (u, v) => u.flatMap(_ => holdOrderIncrease(v)))
	  val nextExecution = 
	    supportMoveOrders.foldLeft(currentExecution)((u, v) => u.flatMap(_ => moveOrderIncrease(v)))  
	  val thirdExecution =
	    supportMoveOrders.foldLeft(nextExecution)((u, v) => u.flatMap(_ => noHelpIncrease(v)))
	  val fourthExecution =
	    regularMoveOrders.foldLeft(thirdExecution)((u, v) => u.flatMap(_ => {
	      var originLocationOption = DBQueries.locations.find(l => l.id == v.dpu.unitLocationID)
	      var dstLocationOption = v.order.dstLocationIDOption.flatMap(dstLocationID => 
	      	DBQueries.locations.find(l => l.id == dstLocationID)
	      )
	      val allExternalPaths = 
	        (for (originLocation <- originLocationOption;
	        		dstLocation <- dstLocationOption
	        ) yield (findAllPathsExternal(originLocation, fleetUnitsForGame))).getOrElse({Nil})
	      val allExternalOrderStates = mapConvoyPathsToFleetUnitOrderStates(allExternalPaths)(orderStates)
	      cutSupport(v)(false)(false)(allExternalOrderStates)(supportOrders)
	    }))
	  
	  val worldState = new WorldState(DBQueries.provinces)
	    
	  val fifthExecution = orderStates.foldLeft(fourthExecution)((u, v) => 
	    for (_ <- fourthExecution;
	    		presentOrder <- v.getPresentOrder();
	    		_ <- addPresentOrderToCombatList(worldState)(v)(presentOrder)
	    ) yield (())
	  )
	  val sixthExecution = moveByConvoyOrders.foldLeft(fifthExecution)((u, v) =>
	    IO(())
	  )
	  
	  
	  
	}
}