/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.squeryl.jdip.schemas

import com.squeryl.jdip.tables._
import org.squeryl._
import org.squeryl.PrimitiveTypeMode._

object Jdip extends Schema {
   val games = table[Games]("games", "jdip")
   val players = table[Players]("players", "jdip")
   val gamePlayerRelations = 
     manyToManyRelation(games, players, "gamePlayerRelations", "jdip").
      via[GamePlayerRelations]((g,p,gp) => 
         (g.id === gp.gameName, p.id === gp.playerName)
       )
   val countries = table[Countries]("countries", "jdip")
   val unitTypes = table[UnitTypes]("unitTypes", "jdip")
   val gameState = table[GameState]("gameState", "jdip")
   

   val orderTypes = table[OrderTypes]("orderTypes", "jdip")
   val orders = table[Orders]("orders", "jdip")
   
   val gamePlayerCountryRelations = 
     manyToManyRelation(gamePlayerRelations, countries, "gamePlayerCountryRelations", "jdip").
      via[GamePlayerCountryRelations]((gpr, c, gpcr) => 
        (gpr.id === gpcr.gamePlayerRelationsId, c.id === gpcr.countryName)
      )
  val phase = table[Phase]("phase", "jdip")
  val season = table[Season]("season", "jdip")
      
  val messages = table[Messages]("messages", "jdip")
  val senderMessageForeignKey = 
    oneToManyRelation(gamePlayerCountryRelations, messages).via((gpcr, m) =>
      gpcr.id === m.senderId
    )
  val receiverMessageForeignKey =
    oneToManyRelation(gamePlayerCountryRelations, messages).via((gpcr, m) =>
      gpcr.id === m.receiverId
    )
 
  val gameNameGameStateForeignKey =
    oneToManyRelation(games, gameState).via((g, gs) => {
        g.id === gs.gameName
    })
  val seasonForeignKey = oneToManyRelation(season, gameState).via((s, gs) => {
    s.id === gs.season
  })
  val phaseForeignKey = oneToManyRelation(phase, gameState).via((p, gs) => {
        p.id === gs.phase
    })

  val gameStateOrdersForeignKey = oneToManyRelation(gameState, orders).via((gs, o) => {
        gs.id === o.gameStateId
    })
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
