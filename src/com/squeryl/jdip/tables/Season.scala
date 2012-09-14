/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.squeryl.jdip.tables

import org.squeryl.KeyedEntity

case class Season(id: String) extends KeyedEntity[String] {
  def this() = this("")
}

sealed abstract class SeasonCaseClass
case class Spring(seasonName: String) extends SeasonCaseClass    
case class Fall(seasonName: String) extends SeasonCaseClass

object Season {
  private val SPRING_NAME = "Spring"
  private val FALL_NAME = "Fall"
  val SPRING = Spring(SPRING_NAME)
  val FALL = Fall(FALL_NAME)
  
  
  def getSeasons = SPRING_NAME :: FALL_NAME :: Nil map (new Season(_))
}