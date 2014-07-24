package com.squeryl.jdip.adjudicators

import com.squeryl.jdip.tables.DiplomacyUnit
import com.squeryl.jdip.tables.Order
import scalaz.effect.IO._
import scalaz.effect._
import com.squeryl.jdip.tables.OrderType
import java.sql.Timestamp

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
  val NO_CONVOY: MARK = "no convoy"
  val BOUNCE: MARK = "bounce"
  val DISLODGED: MARK = "dislodged"
}

class OrderState(val dpu: DiplomacyUnit, val order: Order) {
  private val orderState: IO[IORef[OrderState.RESOLUTION_STATE]] = newIORef({OrderState.UNRESOLVED})
   private val supportCount: IO[IORef[Int]] = newIORef({0})
   private val helpList: IO[IORef[List[OrderState]]] = newIORef({Nil})
   private val presentOrder: IO[IORef[Order]] = newIORef({order})
   private val noHelpList: IO[IORef[List[OrderState]]] = newIORef({Nil})
   private val mark: IO[IORef[OrderState.MARK]] = newIORef({OrderState.NO_MARK})
   
   def setPresentOrderToHold(): IO[Unit] = 
     presentOrder.flatMap(_.write(Order(dpu.id, OrderType.HOLD, new Timestamp(0L), None, None)))
   def getPresentOrder(): IO[Order] = presentOrder.flatMap(_.read)
   def increaseSupportCount(supportingOrderState: OrderState): IO[Int] =
     for (	scIORef <- supportCount; 
		 	hlIORef <- helpList; 
		 	_ <- hlIORef.mod(supportingOrderState :: _); 
		 	newSCCount <- scIORef.mod(_ + 1)
     ) yield newSCCount
   def addNoHelpingUnit(supportingOrderState: OrderState): IO[Unit] =
     noHelpList.flatMap(_.mod(supportingOrderState :: _)).map(_ => ())
   def setMark(newMark: OrderState.MARK): IO[Unit] = mark.flatMap(markIORef =>
     markIORef.write({newMark})
   )
   def getMark(): IO[OrderState.MARK] = mark.flatMap(_.read)
   def getSupportCount(): IO[Int] = supportCount.flatMap(_.read)
   def setSupportCountToZero(): IO[Unit] = supportCount.flatMap(_.write({0}))
   
   def getHelpList(): IO[List[OrderState]] = helpList.flatMap(_.read)
   def setNoHelpListToEmpty(): IO[Unit] = noHelpList.flatMap(_.write({Nil}))
}