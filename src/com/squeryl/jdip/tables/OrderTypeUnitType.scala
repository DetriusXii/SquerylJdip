package com.squeryl.jdip.tables
import org.squeryl.KeyedEntity
import org.squeryl.dsl._
import org.squeryl.PrimitiveTypeMode._

case class OrderTypeUnitType(orderType: String, 
    unitType: String) extends KeyedEntity[CompositeKey2[String, String]] {
	
  def this() = this("", "")

  def id = compositeKey(orderType, unitType)
}

object OrderTypeUnitType {
  def getOrderTypeUnitTypes = {
	val nonConvoyOrderTypes = OrderType.CONSTRUCT :: OrderType.DISBAND :: OrderType.HOLD ::
		OrderType.MOVE :: OrderType.RETREAT :: OrderType.SUPPORT_HOLD :: OrderType.SUPPORT_MOVE :: Nil
    
	(OrderType.CONVOY, UnitType.FLEET) ::
		(for (unitType <- UnitType.ARMY :: UnitType.FLEET :: Nil;
         nonConvoyOrderType <- nonConvoyOrderTypes) yield ((nonConvoyOrderType, unitType))) map (u =>
           new OrderTypeUnitType(u._1, u._2)
         )
  }
}		 