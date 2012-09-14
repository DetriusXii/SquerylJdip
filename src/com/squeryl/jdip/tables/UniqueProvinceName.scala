package com.squeryl.jdip.tables

import org.squeryl.KeyedEntity
import org.squeryl.dsl._
import org.squeryl.PrimitiveTypeMode._

case class UniqueProvinceName(alternateName: String, 
    provinceName: String) extends KeyedEntity[CompositeKey2[String, String]] {
	def this() = this("", "")
	
	def id = compositeKey(alternateName, provinceName)
}