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
import scalaz.ReaderT
import scalaz.Identity
import com.squeryl.jdip.queries.DBQueries

object Main {
	
  /**
   * @param args the command line arguments
   */
  
  private def insertIntoTables(username: String, 
      password: String, configFilepath: String, 
      conn: java.sql.Connection): Unit = {
    transaction {
      EmpireCreator.empireList map (Jdip.empires.insert(_))
      PlayerCreator.playersList map (Jdip.players.insert(_))
      
      
      Jdip.players.insert(new Player("DetriusXii", password))
      Season.getSeasons map (Jdip.seasons.insert(_))
      Phase.getPhases map (Jdip.phases.insert(_))
      
      GameTime.getGameTimes.map(Jdip.gameTimes.insert(_))
      
      UnitType.getUnitTypes map (Jdip.unitTypes.insert(_))
      OrderType.getOrderTypes map (Jdip.orderTypes.insert(_))
      GameState.getGameStates map (Jdip.gameStates.insert(_))
    
      OrderTypeUnitType.getOrderTypeUnitTypes map (Jdip.orderTypeUnitTypes.insert(_))
      Coast.getCoasts map (Jdip.coasts.insert(_))
      
      
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
              gameTime <- Jdip.gameTimes.find(gt => gt.id == g1.gameTime)
          ) yield {
            update(Jdip.games)((g: Game) => where(g.id === g1.id)
                set(g.gameState := GameState.ACTIVE))
            
            val diplomacyUnits = DiplomacyUnitCreator.getDiplomacyUnits(gamePlayerEmpires,
                Jdip.empires,
                UnitType.getUnitTypes,
                Jdip.locations.toList,
            	gameTime, ConfigXMLLoader.findFirstVariant(configFilepath))
            diplomacyUnits map (u => Jdip.diplomacyUnits.insert(u))
            ()
          }  
        }
      
      val ownedProvinces = OwnedProvince.getOwnedProvinces(
          Jdip.diplomacyUnits.toList, 
          Jdip.locations.toList).map(Jdip.ownedProvinces.insert(_))
          
      val dbQueries = new DBQueries(conn)
      val activeGames: List[Game] = dbQueries.getAllActiveGames()
      
      activeGames.foreach((g: Game) => {
        val pmoc = new PotentialMoveOrderCreator(g, dbQueries)
        val pshoc = new PotentialSupportHoldOrderCreator(g, dbQueries)
        val psmoc = new PotentialSupportMoveOrderCreator(g, dbQueries)
        val pcoc = new PotentialConvoyOrderCreator(g, dbQueries)
        
        Jdip.potentialMoveOrders.insert(pmoc.createPotentialMoveOrders)
        Jdip.potentialSupportHoldOrders.
          insert(pshoc.createPotentialSupportHoldOrders)
        Jdip.potentialSupportMoveOrders.
          insert(psmoc.createPotentialSupportMoveOrders)
        Jdip.potentialConvoyOrders.
          insert(pcoc.createPotentialConvoyOrders)
      })
    }
  }
  
  private def insertIntoTablesReader: ReaderT[Identity, Configuration, Unit] =
    ReaderT((c: Configuration) => 
      Identity(
        insertIntoTables(c.username, c.password, c.configFile, c.connection)))
 
  
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
      val connection = 
        java.sql.DriverManager.getConnection(jdbcURL, username, password)
      Configuration(username, password, configFile, jdbcURL, connection)
    }
    
    
    SessionFactory.concreteFactory = for (
        configuration <- configurationOption
    ) yield (() => Session.create(configuration.connection,
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
    
    try {
	    for (c <- configurationOption; 
	      _ <- handleDropOnlyFlag(dropOnlyFlag)
	    ) yield (
	      insertIntoTablesReader.value(c)    
	    )
    } catch {
      case ex: org.postgresql.util.PSQLException => ex.printStackTrace()
    }
	    
    println("The program terminated successfully")
    sys.exit(0)
  }

  
  
}
