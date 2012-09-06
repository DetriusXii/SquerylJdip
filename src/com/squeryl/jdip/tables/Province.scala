package com.squeryl.jdip.tables

import org.squeryl.KeyedEntity

class Province(val id: String, 
    val fullname: String) extends KeyedEntity[String] {
	def this() = this("", "")
}