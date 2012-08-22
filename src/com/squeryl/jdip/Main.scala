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
import com.squeryl.jdip.creators._
import com.squeryl.jdip.schemas.Jdip
import com.squeryl.jdip.creators.DiplomacyUnitCreator

object Main {
	
  /**
   * @param args the command line arguments
   */
  
  private def insertIntoTables(username: String, 
      password: String, configFilepath: String): Unit = {
    transaction {
      EmpireCreator.empireList map (Jdip.empires.insert(_))
      PlayerCreator.playersList map (Jdip.players.insert(_))
      ProvinceCreator.getProvinceList map (Jdip.provinces.insert(_))
      Jdip.players.insert(new Player("DetriusXii", password))
      Season.getSeasons map (Jdip.seasons.insert(_))
      Phase.getPhases map (Jdip.phases.insert(_))
      
      GameTime.getGameTimes.map(Jdip.gameTimes.insert(_))
      
      UnitType.getUnitTypes map (Jdip.unitTypes.insert(_))
      OrderType.getOrderTypes map (Jdip.orderTypes.insert(_))
      GameState.getGameStates map (Jdip.gameStates.insert(_))
    
      val firstGameTime = from(Jdip.gameTimes)(gt => where(
          gt.gameYear === GameTime.MIN_GAME_YEAR and
          gt.gameSeason === Season.SPRING.seasonName and
          gt.gamePhase === Phase.MOVEMENT
      ) select(gt)).head 
      
      val games = ("game1" :: "game2" :: "game3" :: Nil) map
          ((u: String) => Jdip.games.insert(new Game(u, firstGameTime.id)))
      val game1 = games.find(g => g.id.equals("game1"))
      
      val gamePlayers = (("game1", "player1") :: ("game1", "player2") ::
          ("game1", "player3") :: ("game1", "player4") :: ("game1", "player5") :: 
          ("game1", "DetriusXii") :: ("game1", "player6") :: ("game2", "DetriusXii") :: 
          ("game2", "player1") :: Nil) map
          ((u: Tuple2[String, String]) => 
              Jdip.gamePlayers.insert(new GamePlayer(u._1, u._2)))
          
     val gamePlayersQuery = from(Jdip.gamePlayers)(gp =>
       where(gp.gameName === "game1") select(gp)
      )
      
      
      val ids = gamePlayersQuery map (gpr => gpr.id)
      val selectedEmpires = "Turkey" :: "Austria" :: "Russia" :: 
        "England" :: "Italy" :: "France" :: "Germany" :: Nil
      
      val gamePlayerEmpires = (ids zip selectedEmpires) map
        (u => Jdip.gamePlayerEmpires.insert(new GamePlayerEmpire(u._1, u._2)))
        if (gamePlayersQuery.size == 7) {
          for ( g1 <- game1;
              gameTime <- Jdip.gameTimes.find(gt => gt.id == g1.gameTime)
          ) yield {
            update(Jdip.games)((g: Game) => where(g.id === g1.id)
                set(g.gameState := GameState.ACTIVE))
            
            val diplomacyUnits = DiplomacyUnitCreator.getDiplomacyUnits(gamePlayerEmpires, 
            	gameTime, ConfigXMLLoader.findFirstVariant(configFilepath))
            diplomacyUnits map (u => Jdip.diplomacyUnits.insert(u))
            0
          }  
        }
    }
  }
  
 
  
  def main(args: Array[String]): Unit = {
    
    Class.forName("org.postgresql.Driver")
    
    SessionFactory.concreteFactory = Some(() => {
        Session.create(java.sql.DriverManager.getConnection(
            "jdbc:postgresql:postgres", 
            args(0), 
            args(1)), new RevisedPostgreSqlAdapter)
      })
    
    var usernameOption: Option[String] = None
    var passwordOption: Option[String] = None
    var databaseOption: Option[String] = None
    var configFileOption: Option[String] = None
    var dropOnlyFlag: Option[String] = args.find(_.equals("-dropOnly"))
    
    for ( usernameFlag <- args.find(_.equals("-u"));
          passwordFlag <- args.find(_.equals("-p"));
          configFileFlag <- args.find(_.equals("-c"))
    ) yield {
      usernameOption = Some(args.apply(args.indexOf(usernameFlag) + 1))
      passwordOption = Some(args.apply(args.indexOf(passwordFlag) + 1))
      configFileOption = Some(args.apply(args.indexOf(configFileFlag) + 1))
      Unit
    }
    
    SessionFactory.concreteFactory = for ( 
      username <- usernameOption;
	  password <- passwordOption;
	  configFile <- configFileOption
    ) yield (() => Session.create(
    		java.sql.DriverManager.getConnection("jdbc:postgresql:postgres",
    		  username,
    		  password),
    		  new RevisedPostgreSqlAdapter
		  )
	)
	
	transaction {
      Jdip.drop
    }
    
    usernameOption.flatMap(username => 
      passwordOption.flatMap(password => 
    	configFileOption.flatMap(configFile =>
    		(dropOnlyFlag match {
    		  case Some(u) => None
    		  case None => Some(transaction {
    		    Jdip.create
    		  })
    		}).flatMap((v: Unit) => Some(insertIntoTables(username, password, configFile)))
    	)
    ))
    
    println("The program terminated successfully")
    sys.exit(0)
  }

  
  
}
