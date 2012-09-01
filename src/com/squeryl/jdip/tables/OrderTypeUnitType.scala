package com.squeryl.jdip.tables
import org.squeryl.KeyedEntity

class OrderTypeUnitType(val orderType: String, val unitType: String) extends KeyedEntity[Int] {
	def this() = this("", "")
	val id = 0
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