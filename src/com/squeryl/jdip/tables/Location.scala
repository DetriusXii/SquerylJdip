package com.squeryl.jdip.tables

import org.squeryl.KeyedEntity

case class Location(province: String, coast: String) extends KeyedEntity[Int] {
	val id = 0
	
	def this() = this("", "")
}