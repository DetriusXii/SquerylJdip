package com.squeryl.jdip.queries

import com.squeryl.jdip.tables._
import org.squeryl.PrimitiveTypeMode._
import com.squeryl.jdip.schemas.Jdip
import java.util.Date
import java.sql.Timestamp

object InsertStatements {
	def insertMovementPhaseOrder(dpu: DiplomacyUnit, orderType: String, 
	    simpleOrSrcLocationOption: Option[Location], 
	    advancedDstLocationOption: Option[Location]): Unit = transaction {
	  Jdip.orders.insert(
	      Order(dpu.id, orderType, new Timestamp(new Date().getTime()), 
	    		  simpleOrSrcLocationOption.map(_.id),
	    		  advancedDstLocationOption.map(_.id)
	      ))
	}
}