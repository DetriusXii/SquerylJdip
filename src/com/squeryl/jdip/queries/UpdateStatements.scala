package com.squeryl.jdip.queries

import com.squeryl.jdip.tables._

object UpdateStatements {
	def updateMovementPhaseOrder(dpu: DiplomacyUnit, orderType: String, 
	    simpleOrSrcLocationOption: Option[Location], 
	    advancedDstLocationOption: Option[Location]): Option[Unit] = orderType match {
	  case OrderType.HOLD => transaction {
	    update(Jdip.orders)(o => )
	  }
	  case OrderType.MOVE | OrderType.SUPPORT_HOLD => transaction {}
	  case OrderType.SUPPORT_MOVE | OrderType.CONVOY => transaction {}
	}
}