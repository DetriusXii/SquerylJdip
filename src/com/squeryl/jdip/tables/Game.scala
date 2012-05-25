/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.squeryl.jdip.tables

import org.squeryl.KeyedEntity
import org.squeryl.PrimitiveTypeMode._

class Game(val id: String, 
            val gameYear: Int, 
            val season: String,
            val phase: String,
            val gameState: String, 
            val gameFile: Option[BinaryType]) extends KeyedEntity[String] {
   def this() = this("", 0, Season.SPRING, Phase.MOVEMENT, GameState.WAITING, Some(new BinaryType(0)))
  
  def this(u: String) = this(u, 0, Season.SPRING, Phase.MOVEMENT, GameState.WAITING, Some(new BinaryType(0)))
  
} 
