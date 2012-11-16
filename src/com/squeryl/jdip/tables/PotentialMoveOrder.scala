package com.squeryl.jdip.tables

case class PotentialMoveOrder(diplomacyUnitID: Int, moveLocationID: Int) {
	def this() = this(0,0)
}

