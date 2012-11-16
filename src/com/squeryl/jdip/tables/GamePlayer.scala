/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.squeryl.jdip.tables

import org.squeryl.KeyedEntity
import com.squeryl.jdip.schemas.Jdip

case class GamePlayer(gameName: String, 
                 playerName: String) extends KeyedEntity[Int] {
  def this() = this("", "")  
  
  val id = 0
  
  lazy val empires = Jdip.gamePlayerEmpires.left(this)
}
