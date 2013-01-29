package com.squeryl.jdip.queries

import com.squeryl.jdip.tables._
import org.squeryl.PrimitiveTypeMode._
import com.squeryl.jdip.schemas.Jdip
import java.util.Date
import java.sql.Timestamp

object UpdateStatements {
	def updateMovementPhaseOrder(dpu: DiplomacyUnit, orderType: String, 
	    simpleOrSrcLocationOption: Option[Location], 
	    advancedDstLocationOption: Option[Location]): Unit = transaction {
	  update(Jdip.orders)(o => 
	  	where(o.id === dpu.id)
	  	set(o.orderType := orderType,
	  		o.timestamp := new Timestamp(new Date().getTime()),
	  		o.srcLocationIDOption := simpleOrSrcLocationOption.map(_.id),
	  		o.dstLocationIDOption := advancedDstLocationOption.map(_.id)
	  	)
	  )
	}
}