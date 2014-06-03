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
import com.squeryl.jdip.creators._
import scalaz._
import scalaz.Kleisli._
import com.squeryl.jdip.queries.DBQueries
import com.squeryl.jdip.renderers.JdipSVGRenderer
import com.squeryl.jdip.queries.DeleteStatements
import scalaz.Id._

object Main {
  
  private def insertIntoTables(username: String, 
      password: String, configFilepath: String, 
      conn: () => java.sql.Connection): Unit =
    transaction {
      Jdip.empires.insert(EmpireCreator.empireList)
      Jdip.players.insert(PlayerCreator.playersList)
      
      
      Jdip.players.insert(new Player("DetriusXii", password))
      Jdip.seasons.insert(Season.getSeasons)
      Jdip.phases.insert(Phase.getPhases)
      
      Jdip.gameTimes.insert(GameTime.getGameTimes)
      
      Jdip.unitTypes.insert(UnitType.getUnitTypes)
      Jdip.orderTypes.insert(OrderType.getOrderTypes)
      Jdip.gameStates.insert(GameState.getGameStates)
      
      Jdip.orderTypeUnitTypes.insert(OrderTypeUnitType.getOrderTypeUnitTypes)
     
      Jdip.coasts.insert(Coast.getCoasts)
      
      ConfigXMLLoader.findFirstAdjacency(configFilepath) match {
        case Some(u: scala.xml.Elem) => {
          ProvinceCreator.getProvinces(u) map (Jdip.provinces.insert(_))
          UniqueProvinceNameCreator.getUniqueProvinceNames(u) map (Jdip.uniqueProvinceNames.insert(_))
          val locations = LocationCreator.getLocationList(u) map (Jdip.locations.insert(_))
          AdjacencyCreator.getAdjacencies(u, locations) map (Jdip.adjacencies.insert(_))
        }
        case _ => throw new Exception("Did I crash in the adjacencies?")
      }
      
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
              gameTime <- Jdip.gameTimes.find(gt => gt.id == g1.gameTimeID)
          ) yield {
            update(Jdip.games)((g: Game) => where(g.id === g1.id)
                set(g.gameStateID := GameState.ACTIVE))
            
            val diplomacyUnits = DiplomacyUnitCreator.getDiplomacyUnits(gamePlayerEmpires,
                Jdip.empires,
                UnitType.getUnitTypes,
                Jdip.locations.toList,
            	gameTime, ConfigXMLLoader.findFirstVariant(configFilepath))
            diplomacyUnits map (u => Jdip.diplomacyUnits.insert(u))
            ()
          }  
        }
      
      
    }
  
  private def insertIntoTablesReader: Kleisli[Id, Configuration, Unit] =
    ask[Id, Configuration].
    	map((c: Configuration) => insertIntoTables(c.username, 
    	       c.password, c.configFile, c.connection))
 
  private def populatePotentialOrdersReader: 
    Kleisli[Id, Configuration, Unit] =
      ask[Id, Configuration].map((c: Configuration) => 
        populatePotentialOrders(c.connection))
  
  private def renderSVGImage(c: Configuration): Unit = {
      val combinedSVGFilepathOption =
        ConfigXMLLoader.findFirstCombinedSVG(c.configFile)
      combinedSVGFilepathOption.map((filepath: String) => {
        val jdipSVGRenderer = new JdipSVGRenderer(filepath)
        
        val activeGames = DBQueries.getAllActiveGames()
        activeGames.foreach((g: Game) => {
          DeleteStatements.deleteOwnedProvincesForGame(g)
          val diplomacyUnitsForGame
           = DBQueries.getDiplomacyUnitsForGameAtCurrentGameTime(g)
          val ownedProvincesForGame = 
            OwnedProvince.getOwnedProvinces(diplomacyUnitsForGame, 
              DBQueries.locations)
          
          transaction {
            Jdip.ownedProvinces.insert(ownedProvincesForGame)
          }
          
          val renderedDocument = jdipSVGRenderer.getRenderedDocument(g)
          val gameMap = 
            new GameMap(g.id, 
                g.gameTimeID, 
                renderedDocument.toString.getBytes)
          
          transaction {
            Jdip.gameMaps.insert(gameMap)
          }
        })
      })
      
    }
    
  private def renderSVGImageReader: Kleisli[Id, Configuration, Unit] =
    ask[Id, Configuration].map(c => renderSVGImage(c))
    
  private def populatePotentialOrders(c: () => java.sql.Connection): Unit = {
    val activeGames = DBQueries.getAllActiveGames()
    activeGames.foreach((g: Game) => {
        val pmoc = new PotentialMoveOrderCreator(g)
        val pshoc = new PotentialSupportHoldOrderCreator(g)
        val psmoc = new PotentialSupportMoveOrderCreator(g)
        val pcoc = new PotentialConvoyOrderCreator(g)
        
        transaction {
	        Jdip.potentialMoveOrders.insert(pmoc.createPotentialMoveOrders)
	        Jdip.potentialSupportHoldOrders.
	          insert(pshoc.createPotentialSupportHoldOrders)
	        Jdip.potentialSupportMoveOrders.
	          insert(psmoc.createPotentialSupportMoveOrders)
	        Jdip.potentialConvoyOrders.
	          insert(pcoc.createPotentialConvoyOrders)
        }
      })
    
  }
  
  def main(args: Array[String]): Unit = {
    val fullDriverClassName = "org.postgresql.Driver"
    val jdbcURL = "jdbc:postgresql:postgres"
    
    Class.forName(fullDriverClassName)
    
    var dropOnlyFlag: Option[String] = args.find(_.equals("-dropOnly"))
    
    val configurationOption: Option[Configuration] =
      for ( usernameFlag <- args.find(_.equals("-u"));
          passwordFlag <- args.find(_.equals("-p"));
          configFileFlag <- args.find(_.equals("-c"))
    ) yield {
      val username = args(args.indexOf(usernameFlag) + 1)
      val password = args(args.indexOf(passwordFlag) + 1)
      val configFile = args(args.indexOf(configFileFlag) + 1)
      Configuration(username, password, configFile, jdbcURL, () => 
        java.sql.DriverManager.getConnection(jdbcURL, username, password))
    }
    
    
    SessionFactory.concreteFactory = for (
        configuration <- configurationOption
    ) yield (() => Session.create(configuration.connection(),
    		  new RevisedPostgreSqlAdapter
		  )
	)
	
	transaction {
      Jdip.drop
    }
    
    def handleDropOnlyFlag(dropOnlyFlag: Option[_]): Option[_] = 
      if (dropOnlyFlag.isDefined) {
        None
      } else {
        Some(transaction { Jdip.create })
      }
    
    for (c <- configurationOption; 
      _ <- handleDropOnlyFlag(dropOnlyFlag)
    ) yield {
      ask[Id, Configuration].
      	flatMap(_ => insertIntoTablesReader).
      	flatMap(_ => populatePotentialOrdersReader).
      	flatMap(_ => renderSVGImageReader).
      	    apply(c)
    }
    
    println("The program terminated successfully")
    sys.exit(0)
  }

  
  
}
