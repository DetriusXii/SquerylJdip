package com.squeryl.jdip.adjudicators

import com.squeryl.jdip.queries.DBQueries
import com.squeryl.jdip.tables._
import scala.collection.immutable.TreeSet
import java.sql.Timestamp
import scalaz.{Order => _, _}
import scalaz.OptionT._
import scalaz.iteratee._
import scalaz.std.list._
import scalaz.std.list.listInstance._
import com.squeryl.jdip.functions._
import scala.collection.immutable.TreeMap
import scalaz.effect._
import scalaz.concurrent.Future
import scalaz.iteratee._
import std.list._
import syntax.traverse._



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
    
  def apply[S](dpu: DiplomacyUnit, order: Order): OrderState[S] = 
    new OrderState[S](dpu, order)
}

sealed class OrderState[S](val dpu: DiplomacyUnit, val order: Order) {
  
   private val orderState: STRef[S, OrderState.RESOLUTION_STATE] = 
     STRef[S](OrderState.UNRESOLVED)
   private val supportCount: STRef[S, Int] = STRef[S](0)
   private val helpList: STRef[S, List[DiplomacyUnit]] = STRef[S](Nil)
   private val presentOrder: STRef[S, Order] = STRef[S](order)
   private val noHelpList: STRef[S, List[DiplomacyUnit]] = 
     STRef[S](Nil)
   private val mark: STRef[S, OrderState.MARK] = 
     STRef[S](OrderState.NO_MARK)
   
   def setPresentOrderToHold(): ST[S, Unit] = 
     presentOrder.write(Order(dpu.id, OrderType.HOLD, new Timestamp(0L), None, None)).map(_ => ())
   def getPresentOrder(): ST[S, Order] = presentOrder.read
   def increaseSupportCount(supportingDpu: DiplomacyUnit): ST[S, Int] =
     for (	_ <- helpList.mod(supportingDpu :: _); 
		 	newSCCount <- supportCount.mod(_ + 1);
		 	count <- newSCCount.read
     ) yield count
   def addNoHelpingUnit(supportingDiplomacyUnit: DiplomacyUnit): ST[S, Unit] =
     noHelpList.mod(supportingDiplomacyUnit :: _).map(_ => ())
   def setMark(newMark: OrderState.MARK): ST[S, Unit] = 
     mark.write({newMark}).map(_ => ())
  
   def getMark(): ST[S, OrderState.MARK] = mark.read
   def getSupportCount(): ST[S, Int] = supportCount.read
}

object WorldState {
  def apply[S](provinces: List[Province]): WorldState[S] = 
    new WorldState[S](provinces)
    
  def makeListT[S, A](stListT: ST[S, List[A]]) =
    ListT[({type l[A] = ST[S, A]})#l, A](stListT)
  def emptyListTST[S, A] = ListT[({type l[A] = ST[S, A]})#l, A](ST[S, List[A]](Nil))
}

sealed class WorldState[S](val provinces: List[Province]) {
  private type ProvinceOrderStateTreeMap = TreeMap[String, ProvinceOrderState]
  private type OrderStateList = STRef[S, List[OrderState[S]]]
  type OrderStateListTST = ListT[({type l[A] = ST[S, A]})#l, OrderState[S]]
  
  private case class ProvinceOrderState(p: Province, orderStateList: OrderStateList)
  implicit private val orderingProvinceOrderState = 
    new scala.math.Ordering[ProvinceOrderState] {
		def compare(x: ProvinceOrderState, y: ProvinceOrderState): Int =
		  x.p.id.compareTo(y.p.id)
  	}
  
  
  
  private val combatTree: ProvinceOrderStateTreeMap = 
    provinces.foldLeft[ProvinceOrderStateTreeMap](new ProvinceOrderStateTreeMap)((u, v) =>
    	u + ((v.id, ProvinceOrderState(v, STRef[S](Nil)))))
    
 
  def addCombatUnitToProvince(province: Province, combatUnit: OrderState[S]): ST[S, Unit] = 
    combatTree.get(province.id).
    	map(_.orderStateList.mod(combatUnit :: _).map(_ => ())
    ).getOrElse({ST[S, Unit](())})
    
  def getCombatListForProvince(province: Province): Option[OrderStateListTST] = 
    combatTree.get(province.id).map(pos => WorldState.makeListT(pos.orderStateList.read))
    
  def getCombatListForProvince(provinceID: String): Option[OrderStateListTST] =
    combatTree.get(provinceID).map(pos => WorldState.makeListT(pos.orderStateList.read))
}


class MovementPhaseAdjudicator[S](game: Game) {
  private def createHoldOrder(dpu: DiplomacyUnit): Order = Order(dpu.id, OrderType.HOLD, new Timestamp(0L), None, None)  
  
  private def areConvoyOrdersAssistingMovingUnit(
      movingUnit: OrderState[S], 
      convoyOrders: List[OrderState[S]], 
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
  
  private def hasConvoyPath(movingUnit: OrderState[S], 
      convoyOrders: List[OrderState[S]], allFleetUnits: List[DiplomacyUnit]): Boolean = {
    
    
    val movingUnitLocationOption = DBQueries.locations.find(_.id == movingUnit.dpu.unitLocationID)
    val allPossibleConvoyPathsOption = movingUnitLocationOption.map(
        com.squeryl.jdip.functions.findAllPathsExternal(_, allFleetUnits))
    
    (for (allPossibleConvoyPaths <- allPossibleConvoyPathsOption
    ) yield (allPossibleConvoyPaths.exists(
        areConvoyOrdersAssistingMovingUnit(movingUnit, convoyOrders, _)))).getOrElse({false})
    
  }
  
  private def isSupportHoldOrderDefendingUnit(sho: OrderState[S], 
		  holdOrders: List[OrderState[S]]): Boolean = 
    sho.order.dstLocationIDOption.map(dstLocationID =>
        holdOrders.exists(_.dpu.unitLocationID == dstLocationID)).getOrElse({false})
 
  private def getSupportHoldOrderDefendingUnit(sho: OrderState[S],
		  holdOrders: List[OrderState[S]]): Option[OrderState[S]] =
    for (dstLocationID <- sho.order.dstLocationIDOption;
		holdOrder <- holdOrders.find(_.dpu.unitLocationID == dstLocationID)
    ) yield holdOrder
      
  private def getSupportMoveOrderSupportingUnit(smo: OrderState[S],
      moveOrders: List[OrderState[S]]): Option[OrderState[S]] = 
	  	for (dstLocationID <- smo.order.dstLocationIDOption;
				  srcLocationID <- smo.order.srcLocationIDOption;
				  dpuOrderState <- moveOrders.find(_.dpu.unitLocationID == srcLocationID)
		 ) yield dpuOrderState
    
  private def isSupportMoveOrderSupportingUnit(smo: OrderState[S],
		  moveOrders: List[OrderState[S]]): Boolean =
		  (for (dstLocationID <- smo.order.dstLocationIDOption;
				  srcLocationID <- smo.order.srcLocationIDOption;
				  dpuOrderState <- moveOrders.find(_.dpu.unitLocationID == srcLocationID);
				  targetLocationID <- dpuOrderState.order.dstLocationIDOption
		  ) yield (targetLocationID == dstLocationID)).getOrElse({false})
  
  
  private def doesConvoyOrderAgreeWithUnitOrder(co: OrderState[S], 
      moveOrdersByConvoy: List[OrderState[S]]): Boolean =
    (for (srcLocationID <- co.order.srcLocationIDOption;
	  		dstLocationID <- co.order.dstLocationIDOption;
	  		dpu <- moveOrdersByConvoy.find(_.dpu.unitLocationID == srcLocationID);
	  		unitTargetLocationID <- dpu.order.dstLocationIDOption 
  	) yield (dstLocationID == unitTargetLocationID)).getOrElse({false})
 
  private def increaseSupportCountOfHoldOrder(holdOrders: List[OrderState[S]])(sho: OrderState[S]): 
	  ST[S, Unit] = 
	    (for (dstLocationID <- sho.order.dstLocationIDOption;
	        holdOS <- holdOrders.find(_.dpu.unitLocationID == dstLocationID))
	    yield (holdOS.increaseSupportCount(sho).map(_ => ()))).getOrElse(sho.setPresentOrderToHold)
  
  private def increaseSupportCountOfMoveOrder(moveOrders: List[OrderState[S]])(smo: OrderState[S]):
	  ST[S, Unit] =
	    (for (targetLocationID <- smo.order.dstLocationIDOption;
	        srcLocationID <- smo.order.srcLocationIDOption;
	    	moveOS <- moveOrders.find(_.dpu.unitLocationID == srcLocationID);
	    	dstLocationID <- moveOS.order.dstLocationIDOption;
	    	_ <- Some(moveOS) if targetLocationID == srcLocationID
	    ) yield (moveOS.increaseSupportCount(smo).map(_ => ()))).getOrElse(smo.setPresentOrderToHold)
	
  private def increaseNoHelpList[S](moveOrders: List[OrderState[S]], 
      allOrders: List[OrderState[S]])(smo: OrderState[S]): ST[S, Unit] =
    (for (targetLocationID <- smo.order.dstLocationIDOption;
    	srcLocationID <- smo.order.srcLocationIDOption;
    	moveOS <- moveOrders.find(_.dpu.unitLocationID == srcLocationID);
    	dstLocationID <- moveOS.order.dstLocationIDOption;
    	_ <- Some(moveOS) if targetLocationID == srcLocationID;
    	_ <- allOrders.find(otherOrder =>
    	  otherOrder.dpu.unitLocationID == targetLocationID && 
    	  otherOrder.dpu.gamePlayerEmpireID == smo.dpu.gamePlayerEmpireID)
    ) yield (moveOS.addNoHelpingUnit(smo.dpu))).getOrElse({ST[S, Unit](())})
  
  private def handleCutSupportHelper[S](supportOS: OrderState[S], moveOS: OrderState[S], 
      supportOSPresentOrder: Order): IO[Unit] = 
        (for (_ <- Some(()) if supportOSPresentOrder.orderType.compareTo(OrderType.HOLD) != 0
            
        ) yield IO()).getOrElse(IO())
  
  def makeEnumerator[S](xs: List[OrderState[S]]): Enumerator[OrderState[S]] = 
    Iteratee.enumList[OrderState[S], Id.Id](xs)
    
  def exists[S](predicate: OrderState[S] => Boolean): Iteratee[OrderState[S], Boolean] = {
	def step(wasFound: Boolean): Input[OrderState[S]] => Iteratee[OrderState[S], Boolean] = {
	  case Input.Element(orderState) if predicate(orderState) => Iteratee.done(true, Iteratee.eofInput[OrderState[S]])
	  case Input.Element(orderState) => Iteratee.cont(step(false))
	  case Input.Eof() => Iteratee.done(false, Iteratee.eofInput[OrderState[S]])
	  case Input.Empty() => Iteratee.done(false, Iteratee.eofInput[OrderState[S]])
	} 
    Iteratee.cont(step(false))
  }
  
  type ForallST[A] = Forall[({type M[S] = ST[S, A]})#M]
  /*implicit def richList[A, S](l: List[ST[S, A]]) =
    new {
	  def sequence: ST[S, List[A]] = 
	    l.foldLeft
	    
	    l.foldLeft(ST.newVar[List[A]](Nil))(
	      (listSTRef: STRef, elemG: ST[S, A]) => 
      		for (list <- listG;
      				elem <- elemG
      		) yield elem :: list).map(_.reverse)
  	}*/
  

  type STOptionT[S, A] = OptionT[({type l[B] = ST[S, B]})#l, A]
  
  def cutSupport[S](moveOS: OrderState[S])
  	(isStepNineOfAlgorithm: Boolean)
  	(isMoveByConvoy: Boolean)
  	(fleetUnitsOnPath:  List[List[OrderState[S]]])
  	(supports: List[OrderState[S]]): STOptionT[S, Unit] = {
    
	  	def findPathsNotInvolvingSupportUnit(
	  	    supportedFleetUnitLocationID: Int): ST[S, List[List[(Order, OrderState[S])]]] = 
	  	      fleetUnitsOnPath.map(_.map(os => os.getPresentOrder.map((_, os)))).
	  	      map(_.sequence).sequence
	  			
	  	def getSupportedUnit(supportOS: OrderState[S]): Option[Int] = 
	  	  if (supportOS.order.orderType.compareTo(OrderType.SUPPORT_HOLD) == 0)
	  	    supportOS.order.dstLocationIDOption
	  	  else
	  	    supportOS.order.srcLocationIDOption      
	  			
	  	def findSupportOS: Option[OrderState[S]] = 
	  	  for (moveDSTLocationID <- moveOS.order.dstLocationIDOption;
	  	    supportOS <- supports.find(_.dpu.unitLocationID == moveDSTLocationID);
	  	    _ <- Some(supportOS) if 
	  	    	!isSupportOwnedBySamePowerAsMovingUnit(supportOS)
	  	  ) yield supportOS
	  	  
	  	def isSupportCut(supportOSPresentOrder: Order) = 
	  	  supportOSPresentOrder.orderType.compareTo(OrderType.HOLD) == 0
	  	def isSupportOwnedBySamePowerAsMovingUnit(supportOS: OrderState[S]) =
	  	  supportOS.dpu.gamePlayerEmpireID == moveOS.dpu.gamePlayerEmpireID
	  	def isSupportingUnitSupportingAllPathsST(supportedFleetUnitLocationID: Int): ST[S, Boolean] = 
	  	  findPathsNotInvolvingSupportUnit(supportedFleetUnitLocationID).map(_.size == 0)
	  	def isSupportingUnitOfferingSupportToMoveSpace(supportedUnitLocationID: Int) = 
	  	  moveOS.order.dstLocationIDOption.map(_ == supportedUnitLocationID).getOrElse({false})
	  	
  	  	  def pureOptionT[A](a: A): OptionT[({type l[G] = ST[S, G]})#l, A] = 
		    OptionT[({type l[G] = ST[S, G]})#l, A](ST[S, Option[A]](Some(a)))
		  def wrapOption[A](optionA : Option[A]): OptionT[({type l[G] = ST[S, G]})#l, A] =
		    OptionT[({type l[G] = ST[S, G]})#l, A](ST[S, Option[A]](optionA))
		  def wrapOptionST[A](aOptionST: ST[S, Option[A]]): OptionT[({type l[G] = ST[S, G]})#l, A] =
		    OptionT[({type l[G] = ST[S, G]})#l, A](aOptionST)
		  def wrapST[A](aST: ST[S, A]) = wrapOptionST(aST.map(Option(_)))

	  	  
	  	def handleCutSupportHelper(supportOS: OrderState[S], 
	  	    supportOSPresentOrder: Order): OptionT[({type l[G] = ST[S, G]})#l, Unit] = 
	  	  (for (_ <- pureOptionT(()) if !isSupportCut(supportOSPresentOrder);
  			  supportedUnitLocationID <- wrapOption(getSupportedUnit(supportOS));
  			  isSupportingUnitSupportingAllPaths <- 
  			  	wrapST(isSupportingUnitSupportingAllPathsST(supportedUnitLocationID));
  			  _ <- pureOptionT(()) if !isSupportingUnitSupportingAllPaths;
  			  _ <- pureOptionT(()) if 
  			  		isStepNineOfAlgorithm || 
  			  		!isSupportingUnitOfferingSupportToMoveSpace(supportedUnitLocationID);
  			  _ <- wrapST(moveOS.setPresentOrderToHold)
	  	  ) yield ())
	  	      
	  	
	  	wrapST(moveOS.getPresentOrder()).flatMap(_.orderType match {
	  	  case OrderType.HOLD => pureOptionT(())
	  	  case OrderType.MOVE => 
	  	    for ( supportOS <- wrapOption(findSupportOS);
	  	    	presentOrder <-	wrapST(supportOS.getPresentOrder);
	  	    	_ <- handleCutSupportHelper(supportOS, presentOrder)
  	    	) yield ()
	  	})
  	}
   
  private def mapConvoyPathsToFleetUnitOrderStates[S]
  	(convoyPaths: List[List[Location]])
  	(orderStates: List[OrderState[S]]): List[List[OrderState[S]]] = 
    convoyPaths.map(_.map(l => orderStates.find(_.dpu.unitLocationID == l.id)).flatten)
  
  private def getMoveByConvoyOrders[S](moveOrders: List[OrderState[S]]): List[OrderState[S]] =
    moveOrders.filter(os => {
	    val unitLocationID = os.dpu.unitLocationID
	    val dstLocationIDOption = os.order.dstLocationIDOption
	    !dstLocationIDOption.map(dstLocationID => 
	      DBQueries.adjacencies.exists(adj => 
	        adj.srcLocation == unitLocationID &&
	        adj.dstLocation == dstLocationID  
	      )).getOrElse({false})
	  })
	  
  private def getRegularMoveOrders[S](moveOrders: List[OrderState[S]]): List[OrderState[S]] = 
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
  	(moveByConvoyOrders: List[OrderState[S]])
  	(convoyOrders: List[OrderState[S]])
  	(fleetUnitsForGame: List[DiplomacyUnit]): ST[S, Unit] = 
  	    moveByConvoyOrders.map(moveByConvoyOrder =>
	    Future[ST[S, Unit]] {
	      if (!hasConvoyPath(moveByConvoyOrder, convoyOrders, fleetUnitsForGame)) {
	        moveByConvoyOrder.setPresentOrderToHold()
	      } else {
	        ST[S, Unit](())
	      }
	    }
	  ).foldLeft(ST[S, Unit](()))((u, v) => u.flatMap(_ => v.run))
  
  private def evaluateConvoys
  	(convoyOrders: List[OrderState[S]])
  	(moveOrders: List[OrderState[S]]): ST[S, Unit] =
    	  convoyOrders.map(co => 
	  	Future[ST[S, Unit]] {
	  	  if (doesConvoyOrderAgreeWithUnitOrder(co, moveOrders)) 
	  	    ST[S, Unit](())
	  	  else co.setPresentOrderToHold
	  	}
	  ).foldLeft(ST[S, Unit](()))((u, v) => u.flatMap(_ => v.run))
	  
  private def getAllOrderStates(diplomacyUnits: List[DiplomacyUnit]): List[OrderState[S]] =
	  diplomacyUnits.map((dpu: DiplomacyUnit) => {
	    val orderForDpu = DBQueries.getOrderForDiplomacyUnit(dpu).getOrElse(createHoldOrder(dpu))
	    new OrderState[S](dpu, orderForDpu)
	  })
	  
  private def addPresentOrderToCombatList
  	(worldState: WorldState[S])
  	(orderState: OrderState[S])
  	(presentOrder: Order): ST[S, Unit] = {
    def findProvinceByLocationID(locationID: Int): Option[Province] =
      for (location <- DBQueries.locations.find(_.id == locationID);
    		  province <- DBQueries.provinces.find(_.id == location.province)
      ) yield province
   
    def getProvinceForOrder: Option[Province] = presentOrder.orderType match {
        case OrderType.MOVE => presentOrder.dstLocationIDOption.flatMap(dstLocationID => 
      		findProvinceByLocationID(dstLocationID))
        case _ => findProvinceByLocationID(orderState.dpu.unitLocationID)
	
	  }
    
    getProvinceForOrder.map(worldState.addCombatUnitToProvince(_, orderState)).
    	getOrElse({ST[S, Unit](())})
  }
  
  type ListTST[A] = ListT[({type l[A] = ST[S, A]})#l, A]
  
  def checkDisruptions(worldState: WorldState[S])
  	(moveByConvoyOrderState: OrderState[S])
  	(fleetUnitsOnPaths: List[List[OrderState[S]]]): ST[S, Unit] = {
    def getProvinceIDFromLocationID(locationID: Int): Option[String] = 
      DBQueries.locations.find(_.id == locationID).map(_.province) 
    def getCombatListOrderStates(orderState: OrderState[S]) = 
      getProvinceIDFromLocationID(orderState.dpu.unitLocationID).flatMap(provinceID =>
          worldState.getCombatListForProvince(provinceID)).
          getOrElse({ListT.empty[({type l[A] = ST[S, A]})#l, Order]})
      
    val filteredListOfPaths = {
      val sequencedFleetUnitsOnPathListT = WorldState.makeListT(fleetUnitsOnPaths.map(_.
          map(fleetUnitOS => fleetUnitOS.getSupportCount.map((_, fleetUnitOS))).sequence).sequence)
      val furtherSequencingRequired = sequencedFleetUnitsOnPathListT.map(_.map(fleetUnitOS => {
        val locationOption = DBQueries.locations.find(_.id == fleetUnitOS._2.dpu .unitLocationID)
        val otherCombatListST = locationOption.flatMap[ListTST[OrderState[S]]](location =>
        	worldState.getCombatListForProvince(location.province)
        ).map(_.filter(_.dpu.unitLocationID != fleetUnitOS._2.dpu.unitLocationID)).getOrElse({
          WorldState.emptyListTST[S, OrderState[S]]
        })
        val combatListWithSupportCountST = otherCombatListST.flatMap(os => 
          WorldState.makeListT(os.getSupportCount.map((_, os) :: Nil)))
        combatListWithSupportCountST.run.map((fleetUnitOS, _))
      }))
      val fullySequencedListOfLists = furtherSequencingRequired.run.flatMap(_.map(_.sequence).sequence)
      WorldState.makeListT(fullySequencedListOfLists).filter(!_.exists(u => 
      	u._2.find(_._1 > u._1._1).isEmpty
      ))
    }
    
    filteredListOfPaths.length.flatMap(_ match {
      case 0 => moveByConvoyOrderState.setMark(OrderState.CONVOY_ENDANGERED)
      case _ => IO()
    })
  }
  
  def step4Execution(moveByConvoyOrders: List[OrderState[S]], 
      worldState: WorldState[S], 
      fleetUnitsOnPath: List[List[OrderState[S]]],
      supports: List[OrderState[S]]): IO[Unit] = moveByConvoyOrders.foldLeft(IO(()))((u, v) =>
    for (_ <- checkDisruptions(worldState)(v)(fleetUnitsOnPath);
    		markForUnit <- v.getMark();
    		_ <- markForUnit match { 
    		  case OrderState.NO_MARK => cutSupport(v)(false)(true)(fleetUnitsOnPath)(supports)
    		  case _ => v.setMark(OrderState.CONVOY_UNDER_ATTACK)
    		}
  )
  
  /*def step5Execution(moveByConvoyOrders: List[OrderState],
      worldState: WorldState,
      fleetUnitsOnPath: List[List[OrderState]],
      supports: List[OrderState]): IO[WorldState] = {
    val initialConvoySucceededLength = worldState.convoySucceededList.length
    
    def handleConvoyEndangeredCase(os: OrderState): IO[Unit] = 
      os.setMark(OrderState.NO_CONVOY).flatMap(_ => os.setSupportCountToZero()).flatMap(_ => os.getHelpList()).
      flatMap(_.foldLeft(IO(()))((io, hOS) => io.flatMap(_ => hOS.getMark()).flatMap(_ match {
        case OrderState.NO_CONVOY => hOS.setMark(OrderState.NO_CONVOY)
        case _ => io
      })))
    
    def handleConvoyUnderAttackCase(os: OrderState, ws: WorldState): IO[WorldState] =
      os.setMark(OrderState.NO_MARK).flatMap(_ => cutSupport(os)(false)(true)(fleetUnitsOnPath)(supports)).
      map(_ => ws.addConvoySucceeded(os))
    
    val finalWorldStateIO = moveByConvoyOrders.foldLeft(IO({worldState}))((wsIO, mco) =>
    	for (ws <- wsIO;
    	    _ <- checkDisruptions(ws)(mco)(fleetUnitsOnPath);
    			markForUnit <- mco.getMark();
    			newWS <- markForUnit match {
    			  case OrderState.CONVOY_ENDANGERED => handleConvoyEndangeredCase(mco).map(_ => ws)
    			  case OrderState.CONVOY_UNDER_ATTACK => handleConvoyUnderAttackCase(mco, ws)
    			}
    	) yield newWS
    )
    
    finalWorldStateIO.flatMap(ws => ws.convoySucceededList.length > initialConvoySucceededLength match {
      case true => step4Execution(moveByConvoyOrders, ws, fleetUnitsOnPath, supports)
      case false => finalWorldStateIO
    })
  }
  
  def step6Execution(regularMoveOrders: List[OrderState], 
      worldState: WorldState): IO[Unit] = {
    def findSwapper(regularMoveOrder: OrderState, 
        allMoveOrders: List[OrderState]): Option[OrderState] = 
      allMoveOrders.find(otherOrder => 
        (for (otherOrderMoveLocationID <- otherOrder.order.dstLocationIDOption;
        	thisMoveLocationID <- regularMoveOrder.order.dstLocationIDOption
        ) yield (otherOrderMoveLocationID == regularMoveOrder.dpu.unitLocationID &&
        		thisMoveLocationID == otherOrder.dpu.unitLocationID
        )).getOrElse({false})
	  )
    
	
	
    val filteredMoveOrdersIO =
      sequence(regularMoveOrders.map(mo => mo.getMark().map((_, mo)))).
      map(_.filter(_._1.compareTo(OrderState.NO_MARK) == 0).map(_._2))
      
    for (filteredMoveOrders <- filteredMoveOrders;
    	
    )
  }*/
	  
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
	  ))
	  
	  
	  
	}
}
