package com.squeryl.jdip.tables

import org.squeryl.KeyedEntity
import org.squeryl.dsl._
import org.squeryl.PrimitiveTypeMode._

class UniqueProvinceName(val alternateName: String, 
    val provinceName: String) extends KeyedEntity[CompositeKey2[String, String]] {
	def this() = this("", "")
	
	def id = compositeKey(alternateName, provinceName)
}