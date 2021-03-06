/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.squeryl.jdip.tables

import org.squeryl.KeyedEntity
import org.squeryl.PrimitiveTypeMode._
import com.squeryl.jdip.schemas.Jdip

case class Game(id: String, 
			gameTimeID: Int,
            gameStateID: String, 
            gameFile: Option[Array[Byte]]) extends KeyedEntity[String] {
  def this() = this("", 0, GameState.WAITING, Some(new Array[Byte](0)))
  
  def this(u: String, gameTime: Int) = this(u, gameTime, GameState.WAITING, Some(new Array[Byte](0)))
  
  lazy val players = Jdip.gamePlayers.left(this)
  
  lazy val gameMaps = Jdip.gmGameForeignKey.left(this)
  lazy val gameTime = Jdip.gamesGameTimeForeignKey.right(this)
  lazy val gameState = Jdip.gamesGameStateForeignKey.right(this)
} 
