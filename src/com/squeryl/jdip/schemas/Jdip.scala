/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.squeryl.jdip.schemas

import com.squeryl.jdip.tables._
import org.squeryl._
import org.squeryl.PrimitiveTypeMode._

object Jdip extends PostgreSchema("jdip") {
  val players = table[Players]("players", schemaName)
   val phases = table[Phases]("phases", schemaName)
   val seasons = table[Seasons]("seasons", schemaName)
   val games = table[Games]("games", schemaName)
   val orderTypes = table[OrderTypes]("order_types", schemaName)
   val orders = table[Orders]("orders", schemaName)
   val empires = table[Empires]("empires", schemaName)
   val unitTypes = table[UnitTypes]("unit_types", schemaName)
   val gameStates = table[GameStates]("game_states", schemaName)
   val messages = table[Messages]("messages", schemaName)
  
  val gamePlayerRelations = 
     manyToManyRelation(games, players, "game_player_relation", schemaName).
      via[GamePlayerRelations]((g,p,gp) => 
         (g.id === gp.gameName, p.id === gp.playerName)
       ) 
       
  val empiresForeignKey = oneToManyRelation(gamePlayerRelations, empires).via((gpr, e) => 
    e.id === gpr.empireName
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
    oneToManyRelation(gamePlayerRelations, messages).via((gpr, m) =>
      gpr.id === m.senderId
    )
  val receiverMessageForeignKey =
    oneToManyRelation(gamePlayerRelations, messages).via((gpr, m) =>
      gpr.id === m.receiverId
    )
 
  val gamePlayerCountryRelationsOrdersForeignKey = 
    oneToManyRelation(gamePlayerRelations, orders).via((gpr, o) => {
        gpr.id === o.gamePlayerId
    })
  val orderTypeOrdersForeignKey = oneToManyRelation(orderTypes, orders).via((ot, o) => {
        ot.id === o.orderType
    })
  val unitTypesOrdersForeignKey = oneToManyRelation(unitTypes, orders).via((ut, o) => {
        ut.id === o.unitType
    })
}
