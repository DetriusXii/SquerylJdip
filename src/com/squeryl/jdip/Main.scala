/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.squeryl.jdip

import org.squeryl._
import org.squeryl.PrimitiveTypeMode._
import com.squeryl.jdip.adapters.PostgreSqlAdapter
import java.sql.{Array => _, _}
import org.squeryl.dsl.ast._
import com.squeryl.jdip.tables._
import com.squeryl.jdip.schemas.Jdip

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
            args(1)), new PostgreSqlAdapter)
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
      (firstMessage :: secondMessage :: Nil) map (Jdip.messages.insert(_))
      
      update(Jdip.messages)(m =>
        where(m.senderId === russiaGamePlayerCountry.id)
        set(m.message := "Yes.  I will ally with you")
      )
    }
    
    
    
  }

}
