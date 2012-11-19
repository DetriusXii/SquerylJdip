package com.squeryl.jdip.tables

case class PotentialSupportHoldOrder(diplomacyUnitID: Int, supportHoldLocationID: Int) {
	def this() = this(0,0)
}