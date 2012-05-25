/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.squeryl.jdip.tables

import org.squeryl.KeyedEntity

class GamePlayerRelations(val gameName: String, 
                          val playerName: String) extends KeyedEntity[Int] {
  def this() = this("", "")  
  
  val id = 0
}
