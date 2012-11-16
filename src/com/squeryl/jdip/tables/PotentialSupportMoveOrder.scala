package com.squeryl.jdip.tables

case class PotentialSupportMoveOrder(diplomacyUnitID: Int, 
    supportMoveTargetLocationID: Int, supportMoveSourceLocationID: Int) {
	def this() = this(0,0,0)
}