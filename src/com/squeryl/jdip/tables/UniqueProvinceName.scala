package com.squeryl.jdip.tables

import org.squeryl.KeyedEntity

class UniqueProvinceName(val id: String, val provinceName: String) extends KeyedEntity[String] {
	def this() = this("", "")
}