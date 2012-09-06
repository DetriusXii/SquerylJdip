package com.squeryl.jdip.tables

import org.squeryl.KeyedEntity

class Coast(val id: String) extends KeyedEntity[String] {
  def this() = this("")
}

object Coast {
  val ALL_COAST = "xc"
  val NORTH_COAST = "nc"
  val EAST_COAST = "ec"
  val SOUTH_COAST = "sc"
  val WEST_COAST = "wc"
  val NO_COAST = "mv"
    
  def getCoasts: List[Coast] = 
    ALL_COAST :: NORTH_COAST :: EAST_COAST :: SOUTH_COAST :: WEST_COAST :: NO_COAST :: Nil map (new Coast(_))
}