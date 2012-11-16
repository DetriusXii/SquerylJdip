package com.squeryl.jdip.tables

import org.squeryl.KeyedEntity
import com.squeryl.jdip.schemas.Jdip

case class Province(id: String, 
    fullname: String) extends KeyedEntity[String] {
	def this() = this("", "")
	
	lazy val coasts = Jdip.locations.left(this)
}