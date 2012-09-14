package com.squeryl.jdip.tables

import org.squeryl.KeyedEntity

case class Province(id: String, 
    fullname: String) extends KeyedEntity[String] {
	def this() = this("", "")
}