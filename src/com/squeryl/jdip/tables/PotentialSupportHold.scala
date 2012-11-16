package com.squeryl.jdip.tables

case class PotentialSupportHold(diplomacyUnitID: Int, supportHoldLocationID: Int) {
	def this() = this(0,0)
}