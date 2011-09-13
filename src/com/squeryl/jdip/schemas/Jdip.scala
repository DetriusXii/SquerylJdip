/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.squeryl.jdip.schemas

import com.squeryl.jdip.tables._
import org.squeryl._
import org.squeryl.PrimitiveTypeMode._

object Jdip extends Schema {
   val players = table[Players]("players", "jdip")
   val phases = table[Phases]("phases", "jdip")
   val seasons = table[Seasons]("seasons", "jdip")
   val games = table[Games]("games", "jdip")
   val orderTypes = table[OrderTypes]("orderTypes", "jdip")
   val orders = table[Orders]("orders", "jdip")
   val empires = table[Empires]("empires", "jdip")
   val unitTypes = table[UnitTypes]("unitTypes", "jdip")
   val gameStates = table[GameStates]("gameStates", "jdip")
   val messages = table[Messages]("messages", "jdip")
  


   
  val gamePlayerRelations = 
     manyToManyRelation(games, players, "gamePlayerRelations", "jdip").
      via[GamePlayerRelations]((g,p,gp) => 
         (g.id === gp.gameName, p.id === gp.playerName)
       ) 
  val gamePlayerCountryRelations = 
     manyToManyRelation(gamePlayerRelations, empires, "gamePlayerCountryRelations", "jdip").
      via[GamePlayerCountryRelations]((gpr, c, gpcr) => 
        (gpr.id === gpcr.gamePlayerRelationsId, c.id === gpcr.countryName)
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
    oneToManyRelation(gamePlayerCountryRelations, messages).via((gpcr, m) =>
      gpcr.id === m.senderId
    )
  val receiverMessageForeignKey =
    oneToManyRelation(gamePlayerCountryRelations, messages).via((gpcr, m) =>
      gpcr.id === m.receiverId
    )
 
  val gamePlayerCountryRelationsOrdersForeignKey = 
    oneToManyRelation(gamePlayerCountryRelations, orders).via((gpcr, o) => {
        gpcr.id === o.gamePlayerCountryId
    })
  val orderTypeOrdersForeignKey = oneToManyRelation(orderTypes, orders).via((ot, o) => {
        ot.id === o.orderType
    })
  val unitTypesOrdersForeignKey = oneToManyRelation(unitTypes, orders).via((ut, o) => {
        ut.id === o.unitType
    })

}
