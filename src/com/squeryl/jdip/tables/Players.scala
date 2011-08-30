/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.squeryl.jdip.tables

class Players(val playerName: String) extends KeyedEntity[String] {
  def this() = this("")
  
  val id = playerName
  
  lazy val games = Jdip.gamePlayerRelations.right(this)
}