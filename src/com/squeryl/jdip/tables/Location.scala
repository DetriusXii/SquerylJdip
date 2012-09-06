package com.squeryl.jdip.tables

import org.squeryl.KeyedEntity

class Location(val province: String, val coast: String) extends KeyedEntity[Int] {
	val id = 0
	
	def this() = this("", "")
}