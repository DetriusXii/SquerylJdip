/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.squeryl.jdip

import org.squeryl._
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.adapters.PostgreSqlAdapter
import org.squeryl.dsl.CompositeKey2
import java.sql.{Array => _, _}
import org.squeryl.dsl.CompositeKey3
import org.squeryl.dsl.CompositeKey4
import org.squeryl.dsl.ast._
import org.squeryl.internals.FieldMetaData

object Jdip extends Schema {
   val games = table[Games]("games", "jdip")
   val players = table[Players]("players", "jdip")
   val gamePlayerRelations = 
     manyToManyRelation(games, players, "gamePlayerRelations", "jdip").via[GamePlayerRelations](
       (g,p,gp) => {
         (g.id === gp.gameName, p.id === gp.playerName)
       }
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
  val seasonForeignKey = oneToManyRelation(season, gameState).via((s, gs) =>
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
        gpcr.id === o.gamePlayerCountryRelationsId
    })
  val orderTypeOrdersForeignKey = oneToManyRelation(orderType, orders).via((ot, o) => {
        ot.id === o.orderType
    })
  val unitTypesOrdersForeignKey = oneToManyRelation(unitType, orders).via((ut, o) => {
        ut.id === o.unitType
    })

}

object Main {

  /**
   * @param args the command line arguments
   */
  def main(args: Array[String]): Unit = {
    println("Hello, world!")
    
    Class.forName("org.postgresql.Driver")
    
    SessionFactory.concreteFactory = Some(() => {
        Session.create(java.sql.DriverManager.getConnection(
            "jdbc:postgresql:postgres", 
            args(0), 
            args(1)), new PostgreSqlAdapter {
            override def writeUniquenessConstraint(t: Table[_], cols: Iterable[FieldMetaData]) = {
              //ALTER TABLE TEST ADD CONSTRAINT NAME_UNIQUE UNIQUE(NAME)
              val sb = new StringBuilder(256)

              sb.append("alter table ")
              sb.append(quoteName(t.prefixedName))
              sb.append(" add constraint ")
              sb.append(quoteName(t.name + "CPK"))
              sb.append(" unique(")
              sb.append(cols.map(_.columnName).map(quoteName(_)).mkString(","))
              sb.append(")")
              sb.toString
            }
          })
      })
    
    transaction {
      Jdip.drop
    }
    
    transaction {
      Jdip.create
    }
    
    transaction {
      ("Austria" :: "England" :: "France" :: "Germany" :: 
       "Italy" :: "Russia" :: "Turkey" :: Nil) map ((u: String) => Jdip.countries.insert(new Countries(u)))
      ("Spring" :: "Fall" :: Nil) map ((u: String) => Jdip.season.insert(new Season(u)))
      ("Movement" :: "Retreat" :: "Build" :: Nil) map ((u: String) => Jdip.phase.insert(new Phase(u)))
      ("Army" :: "Fleet" :: Nil) map ((u: String) => Jdip.unitTypes.insert(new UnitTypes(u)))
      ("Move" :: "Support Move" :: "Support Hold" :: 
       "Convoy" :: "Hold" :: "Construct" :: "Disband" :: Nil) map 
        ((u:String) => Jdip.orderTypes.insert(new OrderTypes(u)))
      
    }
    
    transaction {
       val games = ("game1" :: "game2" :: "game3" :: Nil) map
          ((u: String) => Jdip.games.insert(new Games(u)))
       val players = ("player1" :: "player2" :: 
          "player3" :: "player4" :: "player5" :: 
          "player6" :: "player7" :: Nil) map ((u: String) => 
              Jdip.players.insert(new Players(u)))
       val gamePlayerRelations = (("game1", "player1") :: ("game1", "player2") ::
          ("game1", "player3") :: ("game1", "player4") :: ("game1", "player5") :: Nil) map
          ((u: Tuple2[String, String]) => 
              Jdip.gamePlayerRelations.insert(new GamePlayerRelations(u._1, u._2)))
          
       val gamePlayerRelationsQuery = from(Jdip.gamePlayerRelations)(gpr =>
         where(gpr.gameName === "game1") select(gpr)
        )
      
      
       val ids = gamePlayerRelationsQuery map (gpr => gpr.id)
       val selectedCountries = gamePlayerRelationsQuery.count(p => true) match {
        case 1 => "Turkey" :: Nil
        case 2 => "Turkey" :: "Austria" :: Nil
        case 3 => "Turkey" :: "Austria" :: "Russia" :: Nil
        case 4 => "Turkey" :: "Austria" :: "Russia" :: "England" :: Nil
        case 5 => "Turkey" :: "Austria" :: "Russia" :: "England" :: "Italy" :: Nil
        case 6 => "Turkey" :: "Austria" :: "Russia" :: "England" :: "Italy" :: "France" :: Nil
        case 7 => "Turkey" :: "Austria" :: "Russia" :: "England" :: "Italy" :: "France" :: "Germany" :: Nil
        case _ => Nil
      }
      
      val gamePlayerCountryRelations = (ids zip selectedCountries) map
        (u => Jdip.gamePlayerCountryRelations.insert(new GamePlayerCountryRelations(u._1, u._2)))
      val austriaGamePlayerCountry = 
        Jdip.gamePlayerCountryRelations.where(gpcr => gpcr.countryName === "Austria").single
      val russiaGamePlayerCountry = 
        Jdip.gamePlayerCountryRelations.where(gpcr => gpcr.countryName === "Russia").single
   
      val firstMessageDate = new java.util.Date
      
      
      val firstMessage = new Messages(
        austriaGamePlayerCountry.id, 
        russiaGamePlayerCountry.id, 
        new Timestamp(firstMessageDate.getTime), 
        "Hi Russia. Want to ally?"
      )
      
      val secondMessageDate = new java.util.Date
      val secondMessage = new Messages(russiaGamePlayerCountry.id, 
                                       austriaGamePlayerCountry.id, 
                                       new Timestamp(secondMessageDate.getTime), 
                                       "No. I will destroy your country and take your lands")
      (firstMessage :: secondMessage :: Nil) map (u => Jdip.messages.insert(u))
      
      update(Jdip.messages)(m =>
        where(m.senderId === russiaGamePlayerCountry.id)
        set(m.message := "Yes.  I will ally with you")
      )
    }
    
    
    
  }

}
