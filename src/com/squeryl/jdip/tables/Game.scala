/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.squeryl.jdip.tables

import org.squeryl.KeyedEntity
import org.squeryl.PrimitiveTypeMode._
import com.squeryl.jdip.schemas.Jdip

case class Game(id: String, 
			gameTime: Int,
            gameState: String, 
            gameFile: Option[BinaryType]) extends KeyedEntity[String] {
  def this() = this("", 0, GameState.WAITING, Some(new BinaryType(0)))
  
  def this(u: String, gameTime: Int) = this(u, gameTime, GameState.WAITING, Some(new BinaryType(0)))
  
  lazy val players = Jdip.gamePlayers.left(this)
  
  lazy val gameMaps = Jdip.gmGameForeignKey.left(this)
} 
