/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.squeryl.jdip.schemas

import com.squeryl.jdip.tables._
import org.squeryl._
import org.squeryl.PrimitiveTypeMode._

object Jdip extends PostgreSchema("jdip") {
   val players = table[Player]("players", schemaName)
   val phases = table[Phase]("phases", schemaName)
   val seasons = table[Season]("seasons", schemaName)
   val games = table[Game]("games", schemaName)
   val orderTypes = table[OrderType]("order_types", schemaName)
   val orders = table[Order]("orders", schemaName)
   val empires = table[Empire]("empires", schemaName)
   val unitTypes = table[UnitType]("unit_types", schemaName)
   val gameStates = table[GameState]("game_states", schemaName)
   val messages = table[Message]("messages", schemaName)
  
  val gamePlayers = 
     manyToManyRelation(games, players, "game_player", schemaName).
      via[GamePlayer]((g,p,gp) => 
         (g.id === gp.gameName, p.id === gp.playerName)
       ) 
       
  val gamePlayerEmpires = manyToManyRelation(gamePlayers, empires, "game_player_empire", schemaName).
    via[GamePlayerEmpire]((gamePlayers, empires, gpe) => 
      (gamePlayers.id === gpe.gamePlayerID, empires.id === gpe.empireName)
    )
       
  
  val gamesGameStateForeignKey = oneToManyRelation(gameStates, games).via((gs, g) => {
       gs.id === g.gameState
    })
   val gamesPhasesForeignKey = oneToManyRelation(phases, games).via((p, g) => {
       p.id === g.phase
    })
  val gamesSeasonsForeignKey = oneToManyRelation(seasons, games).via((s, g) => {
      s.id === g.season
    })
  
  val senderMessageForeignKey = 
    oneToManyRelation(gamePlayers, messages).via((gpr, m) =>
      gpr.id === m.senderId
    )
  val receiverMessageForeignKey =
    oneToManyRelation(gamePlayers, messages).via((gpr, m) =>
      gpr.id === m.receiverId
    )
 
  val gamePlayerCountryRelationsOrdersForeignKey = 
    oneToManyRelation(gamePlayers, orders).via((gpr, o) => {
        gpr.id === o.gamePlayer
    })
  val orderTypeOrdersForeignKey = oneToManyRelation(orderTypes, orders).via((ot, o) => {
        ot.id === o.orderType
    })
  val unitTypesOrdersForeignKey = oneToManyRelation(unitTypes, orders).via((ut, o) => {
        ut.id === o.unitType
    })
}
