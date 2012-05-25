/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.squeryl.jdip

import org.squeryl._
import org.squeryl.PrimitiveTypeMode._
import com.squeryl.jdip.adapters.RevisedPostgreSqlAdapter
import java.sql.{Array => _, _}
import org.squeryl.dsl.ast._
import com.squeryl.jdip.tables._
import com.squeryl.jdip.creators.EmpireCreator
import com.squeryl.jdip.creators.PlayerCreator
import com.squeryl.jdip.schemas.Jdip

object Main {
	
  /**
   * @param args the command line arguments
   */
  def main(args: Array[String]): Unit = {
    
    Class.forName("org.postgresql.Driver")
    
    SessionFactory.concreteFactory = Some(() => {
        Session.create(java.sql.DriverManager.getConnection(
            "jdbc:postgresql:postgres", 
            args(0), 
            args(1)), new RevisedPostgreSqlAdapter)
      })
    
    transaction {
      Jdip.drop
    }
    
    transaction {
      Jdip.create
    }
    
    transaction {
      EmpireCreator.empireList map ((u: Empire) => Jdip.empires.insert(u))
      PlayerCreator.playersList map ((u: Player) => Jdip.players.insert(u))
      Jdip.players.insert(new Player("DetriusXii", args(1)))
      Season.getSeasons map (Jdip.seasons.insert(_))
      Phase.getPhases map (Jdip.phases.insert(_))
      UnitType.getUnitTypes map (Jdip.unitTypes.insert(_))
      OrderType.getOrderTypes map (Jdip.orderTypes.insert(_))
      GameState.getGameStates map (Jdip.gameStates.insert(_))
      val games = ("game1" :: "game2" :: "game3" :: Nil) map
          ((u: String) => Jdip.games.insert(new Game(u)))
    }
    
    transaction {
       val gamePlayers = (("game1", "player1") :: ("game1", "player2") ::
          ("game1", "player3") :: ("game1", "player4") :: ("game1", "player5") :: 
          ("game1", "DetriusXii") :: ("game1", "player6") :: ("game2", "DetriusXii") :: 
          ("game2", "player1") :: Nil) map
          ((u: Tuple2[String, String]) => 
              Jdip.gamePlayers.insert(new GamePlayer(u._1, u._2)))
          
       
    }  
     
    transaction {
      val gamePlayersQuery = from(Jdip.gamePlayers)(gp =>
       where(gp.gameName === "game1") select(gp)
      )
      val ids = gamePlayersQuery map (gpr => gpr.id)
      val selectedEmpires = "Turkey" :: "Austria" :: "Russia" :: 
        "England" :: "Italy" :: "France" :: "Germany" :: Nil
      
      val gamePlayerEmpires = (ids zip selectedEmpires) map
        (u => Jdip.gamePlayerEmpires.insert(new GamePlayerEmpire(u._1, u._2)))
        if (gamePlayersQuery.size == 7) {
          update(Jdip.games)((g: Game) => where(g.id === "game1")set(g.gameState := GameState.ACTIVE)
          )
        }
    }
  }

}
