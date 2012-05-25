/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.squeryl.jdip.tables

import org.squeryl.KeyedEntity

class GameState(val id: String) extends KeyedEntity[String] {
  def this() = this("")
  }

object GameState {
  val WAITING = "Waiting for players"
  val COMPLETED = "Game Completed"
  val ACTIVE = "Active"
  val INACTIVE = "Inactive"
  
  def getGameStates = WAITING :: COMPLETED :: ACTIVE :: INACTIVE :: Nil map (new GameState(_))
}