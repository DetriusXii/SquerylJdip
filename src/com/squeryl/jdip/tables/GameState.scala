/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.squeryl.jdip.tables

class GameState(val gameName: String, 
                val gameYear: Int, 
                val season: String, 
                val phase: String,
                val active: Boolean) extends KeyedEntity[Long] {
  def this() = this("", 0, "", "", false)  
  
    val id = 0L
  }
