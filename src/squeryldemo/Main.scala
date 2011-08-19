/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package squeryldemo

import org.squeryl._
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.adapters.PostgreSqlAdapter
import org.squeryl.dsl.CompositeKey2
import java.sql.{Array => _, _}
import org.squeryl.dsl.CompositeKey3
import org.squeryl.dsl.CompositeKey4
import org.squeryl.dsl.ast._

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
}

class Phase(val phase: String) extends KeyedEntity[String] {
  def this() = this("")
  
  val id = phase
}

class Season(val season: String) extends KeyedEntity[String] {
  def this() = this("")
  
  val id = season
}

class GameState(val gameName: String, 
                val gameYear: Int, 
                val season: String, 
                val phase: String,
                val active: ) extends 
  KeyedEntity[CompositeKey4[String,Int,String,String]] {
  def this() = this("", 0, "", "")  
  
    def id = compositeKey(gameName, gameYear, season, phase)
  }
  
class GameMetaState(val metaState: String) extends KeyedEntity[String] {
  def this() = this("")
  
}

class Games(val gameName: String) extends KeyedEntity[String] {
  def this() = this("")
  
  val id = gameName
  
  lazy val players = Jdip.gamePlayerRelations.left(this)
} 

class Players(val playerName: String) extends KeyedEntity[String] {
  def this() = this("")
  
  val id = playerName
  
  lazy val games = Jdip.gamePlayerRelations.right(this)
}

class GamePlayerRelations(val gameName: String, val playerName: String) extends KeyedEntity[Int] {
  def this() = this("", "")  
  
  val id = 0
}

class Countries(val countryName: String) extends KeyedEntity[String] {
  def this() = this("")
  
  val id = countryName
}

class GamePlayerCountryRelations(val gamePlayerRelationsId: Int,
                                 val countryName: String) extends KeyedEntity[Int] {
  def this() = this(0, "")
  
  val id = 0
}

class Messages(val senderId: Int, 
               val receiverId: Int, 
               val timestamp: Timestamp,
               val message: String) extends KeyedEntity[CompositeKey3[Int, Int, java.sql.Timestamp]] {
  def this() = this(0, 0, new Timestamp(0L), "")
  
  def id = compositeKey(senderId, receiverId, timestamp)
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
            "postgres", 
            ""), new PostgreSqlAdapter)
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
