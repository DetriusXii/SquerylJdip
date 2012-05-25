/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.squeryl.jdip.tables

import org.squeryl.KeyedEntity

class Season(val id: String) extends KeyedEntity[String] {
  def this() = this("")
}

object Season {
  val SPRING = "Spring"
  val FALL = "Fall"
  
  def getSeasons = SPRING :: FALL :: Nil map (new Season(_))
}