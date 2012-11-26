package com.squeryl.jdip.tables

import org.squeryl.KeyedEntity
import com.squeryl.jdip.schemas.Jdip

case class Location(province: String, coast: String) extends KeyedEntity[Int] {
	val id = 0
	val presentationName = coast match {
	  case Coast.ALL_COAST | Coast.NO_COAST => province
	  case _ => "%s-%s" format (province, coast)
	}
	
	def this() = this("", "")
	
	lazy val sourceLocations = Jdip.adjacencies.right(this)
	lazy val destinationLocations = Jdip.adjacencies.left(this)
}