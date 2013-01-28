package com.squeryl.jdip.queries

import com.squeryl.jdip.tables._
import org.squeryl.PrimitiveTypeMode._
import com.squeryl.jdip.schemas.Jdip
import java.util.Date

object UpdateStatements {
	def updateMovementPhaseOrder(dpu: DiplomacyUnit, orderType: String, 
	    simpleOrSrcLocationOption: Option[Location], 
	    advancedDstLocationOption: Option[Location]): Option[Unit] = orderType match {
	  case OrderType.HOLD => Some(transaction {
	    update(Jdip.orders)(o => 
	    	where(o.id === dpu.id)
	    	set(o.orderType := orderType, 
	    	    o.timestamp := new java.sql.Timestamp(
	    	        new Date().getTime()),
	    	    o.srcLocationIDOption := None,
	    	    o.dstLocationIDOption := None)
	    )
	  }).map(_ => Unit)
	  case OrderType.MOVE | OrderType.SUPPORT_HOLD =>
	    if (simpleOrSrcLocationOption.isDefined && 
	        advancedDstLocationOption.isEmpty) {
	      Some(transaction {update(Jdip.orders)(o => 
	      	where(o.id === dpu.id)
	      	set(o.orderType := orderType,
	      		o.timestamp := new java.sql.Timestamp(
	      			new Date().getTime()),
	      		o.srcLocationIDOption := 
	      		  simpleOrSrcLocationOption.map(_.id),
	      		o.dstLocationIDOption := None
	      		)
	      	)
	      }).map(_ => Unit)
	    } else {
	      None
	    }
	  case OrderType.SUPPORT_MOVE | OrderType.CONVOY => 
	    if (simpleOrSrcLocationOption.isDefined && 
	    	advancedDstLocationOption.isDefined) {
	      Some(transaction {
	        update(Jdip.orders)(o =>
	          where(o.id === dpu.id)
	          set(o.orderType := orderType,
	              o.srcLocationIDOption :=
	                simpleOrSrcLocationOption.map(_.id),
	              o.dstLocationIDOption :=
	                advancedDstLocationOption.map(_.id)
	          ))
	      })
	    } else {
	      None
	    }
	}
}