package com.squeryl.jdip.queries

import com.squeryl.jdip.tables._
import org.squeryl.PrimitiveTypeMode._
import com.squeryl.jdip.schemas.Jdip
import java.util.Date

object InsertStatements {
	def insertMovementPhaseOrder(dpu: DiplomacyUnit, orderType: String, 
	    simpleOrSrcLocationOption: Option[Location], 
	    advancedDstLocationOption: Option[Location]): Option[Unit] = transaction {
	  orderType match {
	    case OrderType.HOLD => Some(Jdip.orders.insert(Order(dpu.id, 
	        orderType, 
	        new java.sql.Timestamp(new Date().getTime), 
	        None, 
	        None)))
	    case OrderType.MOVE | OrderType.SUPPORT_HOLD => 
	      if (simpleOrSrcLocationOption.isDefined && advancedDstLocationOption.isEmpty) {
	        Some(Jdip.orders.insert(Order(dpu.id, 
	            orderType, 
	            new java.sql.Timestamp(new Date().getTime),
	            simpleOrSrcLocationOption.map(_.id),
	            None)))
	      } else {
	        None
	      }
	    case OrderType.SUPPORT_MOVE | OrderType.CONVOY => 
	      if (simpleOrSrcLocationOption.isDefined && advancedDstLocationOption.isDefined) {
	    	  Some(Jdip.orders.insert(
	    			  Order(dpu.id,  
	    					  orderType, 
	    					  new java.sql.Timestamp(new Date().getTime), 
	    					  simpleOrSrcLocationOption.map(_.id),
	    					  advancedDstLocationOption.map(_.id))))
	      } else {
	        None
	      }
	  }
	}
}