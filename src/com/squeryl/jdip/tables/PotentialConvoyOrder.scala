package com.squeryl.jdip.tables

case class PotentialConvoyOrder(diplomacyUnitID: Int, 
    convoyTargetLocationID: Int, convoySourceLocationID: Int) {
	def this() = this(0,0,0)
}