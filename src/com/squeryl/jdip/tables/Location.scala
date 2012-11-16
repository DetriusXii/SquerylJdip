package com.squeryl.jdip.tables

import org.squeryl.KeyedEntity

case class Location(province: String, coast: String) extends KeyedEntity[Int] {
	val id = 0
	val presentationName = coast match {
	  case Coast.ALL_COAST | Coast.NO_COAST => province
	  case _ => "%s-%s" format (province, coast)
	}
	
	def this() = this("", "")
}