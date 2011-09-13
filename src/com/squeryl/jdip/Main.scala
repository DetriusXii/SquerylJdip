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
    
    val countryDescriptions = """This is a landlocked country caught between Turkey and Russia""" ::
      """This is a western naval country that faces navel aggression from Russia's north fleet and Frances 
  armies to the South""" :: 
      """This western power views England and Germant as enemies and trusts Italy as far as she 
  can throw him""" :: 
    """This industrial superpower has enemies on every side, but is also near various supply centres.  It's in
 a strong position to negotiate its survival""" :: 
    """Italy is a naval power that must uses lies to gain its strength.   It rules in the sea, but 
faces choke points that limits its reach""" :: 
    """A vast country that has borders hard to defend and allies intimidated by the Mother Russia due 
to its size and brutality""" :: 
    """A well defended country that has difficulties becoming the emperor.  Can easily be contained by any two
 of Austria, Russia, or Italy working together.""" :: Nil
    
    transaction {
      (("Austria" :: "England" :: "France" :: "Germany" :: 
       "Italy" :: "Russia" :: "Turkey" :: Nil) zip countryDescriptions) map (
       (u: Tuple2[String, String]) => u._1 match {
         case "Austria" => Jdip.empires.insert(new Empires(u._1, u._2, 1, 2))
         case "England" => Jdip.empires.insert(new Empires(u._1, u._2, 2, 1))
         case "France" => Jdip.empires.insert(new Empires(u._1, u._2, 1, 2))
         case "Germany" => Jdip.empires.insert(new Empires(u._1, u._2, 1, 2))
         case "Italy" => Jdip.empires.insert(new Empires(u._1, u._2, 2, 1))
         case "Russia" => Jdip.empires.insert(new Empires(u._1, u._2, 2, 2))
         case "Turkey" => Jdip.empires.insert(new Empires(u._1, u._2, 1, 2))
        })
      ("Spring" :: "Fall" :: Nil) map ((u: String) => Jdip.seasons.insert(new Seasons(u)))
      ("Movement" :: "Retreat" :: "Build" :: Nil) map ((u: String) => Jdip.phases.insert(new Phases(u)))
      ("Army" :: "Fleet" :: Nil) map ((u: String) => Jdip.unitTypes.insert(new UnitTypes(u)))
      ("Move" :: "Support Move" :: "Support Hold" :: 
       "Convoy" :: "Hold" :: "Construct" :: "Disband" :: Nil) map 
        ((u:String) => Jdip.orderTypes.insert(new OrderTypes(u)))
      ("Waiting for players" :: "Game Completed" :: "Active" :: "Inactive" :: Nil) map
        ((u: String) => Jdip.gameStates.insert(new GameStates(u)))
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
