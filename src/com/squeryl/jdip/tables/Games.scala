/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.squeryl.jdip.tables

class Games(val gameName: String) extends KeyedEntity[String] {
  def this() = this("")
  
  val id = gameName
  
  lazy val players = Jdip.gamePlayerRelations.left(this)
} 
