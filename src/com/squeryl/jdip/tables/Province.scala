package com.squeryl.jdip.tables

import org.squeryl.KeyedEntity

class Province(val id: String, 
    val fullname: String, 
    val isLandProvince: Boolean, 
    val hasCoast: Boolean) extends KeyedEntity[String] {
	def this() = this("", "", false, false)
}