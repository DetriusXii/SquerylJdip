package com.squeryl.jdip.tables

class Province(val id: String, val fullname: String, val isLandProvince: Boolean) extends{
	def this() = this("", "", false)
}