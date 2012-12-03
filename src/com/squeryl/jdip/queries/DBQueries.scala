package com.squeryl.jdip.queries
import com.squeryl.jdip.tables._
import org.squeryl.Session
import com.squeryl.jdip.adapters.RevisedPostgreSqlAdapter
import org.squeryl.PrimitiveTypeMode._
import com.squeryl.jdip.schemas.Jdip
import org.squeryl.Query

class DBQueries(conn: java.sql.Connection) {
  lazy val locations: List[Location] = {
    val dbSession = Session.create(conn, new RevisedPostgreSqlAdapter)
    using(dbSession) {
      Jdip.locations.toList
    }
  }
  
  lazy val uniqueProvinceNames: List[UniqueProvinceName] = {
	val dbSession = Session.create(conn, new RevisedPostgreSqlAdapter)
	using(dbSession) {
	  Jdip.uniqueProvinceNames.toList
	}
  }
    
  lazy val empires: List[Empire] = {
    val dbSession = Session.create(conn, new RevisedPostgreSqlAdapter)
      using(dbSession) {
        Jdip.empires.toList
      }
  }
  
  lazy val adjacencies: List[Adjacency] = {
    val dbSession = Session.create(conn, new RevisedPostgreSqlAdapter)
    using(dbSession) {
      Jdip.adjacencies.toList
    }
  }
  
  lazy val provinces: List[Province] = {
	val dbSession = Session.create(conn, new RevisedPostgreSqlAdapter)
	using(dbSession) {
	  Jdip.provinces.toList
	}
  }
    
  lazy val orderTypeUnitTypes: List[OrderTypeUnitType] = {
	val dbSession = Session.create(conn, new RevisedPostgreSqlAdapter)
	using(dbSession) {
	  Jdip.orderTypeUnitTypes.toList
	}
  }

  lazy val orderTypes: List[OrderType] = {
	val dbSession = Session.create(conn, new RevisedPostgreSqlAdapter)
	using(dbSession) {
	  Jdip.orderTypes.toList
	}
  }

  def getOwnedProvincesForGame(game: Game): List[OwnedProvince] = {
    val dbSession = Session.create(conn, new RevisedPostgreSqlAdapter)
    using(dbSession) {
      from(Jdip.ownedProvinces) (owp =>
        where(owp.gamePlayerEmpireID in gamePlayerEmpireQueryForGame(game) and 
          owp.gameTimeID === game.gameTimeID
        ) 
        select(owp)
      ).toList
    }
  }
  
  def getGame(gameName: String): Option[Game] = {
    val dbSession = Session.create(conn, new RevisedPostgreSqlAdapter)
    using(dbSession) {
      Jdip.games.lookup(gameName)
    }    
  }
    
    
  def getGamePlayerEmpire(gamePlayerEmpireID: Int): Option[GamePlayerEmpire] = {
	val dbSession = Session.create(conn, new RevisedPostgreSqlAdapter)
	using(dbSession) {
	  Jdip.gamePlayerEmpires.lookup(gamePlayerEmpireID)
	}
  }
    
  def getAdjacentLocationsForLocation(loc: Location): List[Location] = {
    adjacencies.filter(_.srcLocation == loc.id).map(adj => {
      locations.find(_.id == adj.dstLocation)
    }).flatten
  }
  
  def getCoastLocationsFromLandLocation(loc: Location): List[Location] = {
    locations.filter(_ match {
      case Location(loc.province, Coast.NO_COAST) => false
      case Location(loc.province, _) => true
      case _ => false
    })
  }
  
  def hasLandLocation(loc: Location): Boolean =
    locations.exists(_ match {
      case Location(loc.province, Coast.NO_COAST) => true
      case _ => false
    })
  
  def getAllLandUnitsForGame(game: Game): List[DiplomacyUnit] = {
	val dbSession = Session.create(conn, new RevisedPostgreSqlAdapter)
	using(dbSession) {
	  from(Jdip.diplomacyUnits)(dpu => (
	    where((dpu.unitType === UnitType.ARMY) and 
	        (dpu.gamePlayerEmpireID in gamePlayerEmpireQueryForGame(game)) and
	        (dpu.gameTimeID === game.gameTimeID)) 
	    select(dpu)
	  )).toList
	}
  }
  
  def getAllFleetUnitsForGame(game: Game): List[DiplomacyUnit] = {
    val dbSession = Session.create(conn, new RevisedPostgreSqlAdapter)
    using(dbSession) {
      from(Jdip.diplomacyUnits)(dpu => (
        where((dpu.unitType === UnitType.FLEET) and 
    	  (dpu.gamePlayerEmpireID in gamePlayerEmpireQueryForGame(game)) and
    	  (dpu.gameTimeID === game.gameTimeID)
        ) 
        select(dpu)
      )).toList
    }
  }
  
  def getDiplomacyUnitsForGameAtCurrentGameTime(game: Game): List[DiplomacyUnit] =
	{
	  val dbSession = Session.create(conn, new RevisedPostgreSqlAdapter)
	  using(dbSession) {
	    from(Jdip.diplomacyUnits)(dpu => 
	      where((dpu.gamePlayerEmpireID in gamePlayerEmpireQueryForGame(game))
	      	and (dpu.gameTimeID === game.gameTimeID))
	      select(dpu)
	    ).toList
	  }
    }
  
  private def gamePlayerEmpireQueryForGame(game: Game): Query[Int] =
    from(Jdip.gamePlayerEmpires, Jdip.gamePlayers)((gpe, gp) => 
    	where(gpe.gamePlayerKey === gp.id and gp.gameName === game.id)
    	select(gpe.id)
    )
  
  def getGamePlayerEmpiresForGame(game: Game): List[GamePlayerEmpire] = {
    val dbSession = Session.create(conn, new RevisedPostgreSqlAdapter)
    using(dbSession) {
      from(Jdip.gamePlayerEmpires) (gpe =>
        where(gpe.id in gamePlayerEmpireQueryForGame(game: Game))
        select(gpe)
      ).toList
    }
  }
  
  def getGameForGamePlayerEmpireID(gamePlayerEmpireID: Int): Option[Game] = {
    val dbSession = Session.create(conn, new RevisedPostgreSqlAdapter)
    using(dbSession) {
      from(Jdip.games, Jdip.gamePlayers, Jdip.gamePlayerEmpires)((g, gp, gpe) => 
      	where((gpe.id === gamePlayerEmpireID) and
    	  (gpe.gamePlayerKey === gp.id) and
          (g.id === gp.gameName)
        )
        select(g)
      ).headOption
    }
  }
    
  def getGameForGamePlayerEmpire(gamePlayerEmpire: GamePlayerEmpire): Option[Game] = {
    val dbSession = Session.create(conn, new RevisedPostgreSqlAdapter)
    using(dbSession) {
      from(Jdip.games, Jdip.gamePlayers, Jdip.gamePlayerEmpires)((g, gp, gpe) => 
      	where((gpe.id === gamePlayerEmpire.id) and
      			(gpe.gamePlayerKey === gp.id) and
      			(g.id === gp.gameName)
      	)
      	select(g)
      ).headOption
    }
  }
  
  def getDiplomacyUnitsForGamePlayerEmpire(gamePlayerEmpire: GamePlayerEmpire):
	  List[DiplomacyUnit] = {
	val dbSession = Session.create(conn, new RevisedPostgreSqlAdapter)
	using(dbSession) {
	  from(Jdip.games, Jdip.gamePlayers, Jdip.diplomacyUnits) ((g, gp, dpu) => 
	    where((gp.id === gamePlayerEmpire.gamePlayerKey) and
	    		(g.id === gp.gameName) and
	    		(dpu.gamePlayerEmpireID === gamePlayerEmpire.id) and
	    		(dpu.gameTimeID === g.gameTimeID)
	    )
	  	select(dpu)
	  ).toList
	}
  }

  def getPlayers: List[Player] = {
    val dbSession = Session.create(conn, new RevisedPostgreSqlAdapter)
    using(dbSession) {
      from(Jdip.players)(select(_)).toList
    }
  }

  def getGamePlayerEmpire(gameName: String, playerName: String): 
	  Option[GamePlayerEmpire] =  {
    val dbSession = Session.create(conn, new RevisedPostgreSqlAdapter)
    using(dbSession) {
      from(Jdip.gamePlayerEmpires) (gpe =>
        where(gpe.gamePlayerKey in from(Jdip.gamePlayers) (gp => 
          where(gp.gameName === gameName and gp.playerName === playerName)
          select(gp.id)))
        select(gpe)
      ).toList.headOption
    }
  }
  
  def getGamesForUser(username: String): List[Game] = {
    val dbSession = Session.create(conn, new RevisedPostgreSqlAdapter)
    using(dbSession) {
      from(Jdip.games) (g =>
        where(g.id in from(Jdip.gamePlayers) (gp => 
          where (gp.playerName like username)
          select(gp.gameName)
          ))
        select(g)
      ).toList
    }
  }
    
   def getGameTimesForGames(gameTimeIDs: List[Int]): List[GameTime] = {
     val dbSession = Session.create(conn, new RevisedPostgreSqlAdapter)
     using(dbSession) {
       from(Jdip.gameTimes) (gt =>
         where(gt.id in gameTimeIDs)
         select(gt)
       ).toList
     }
   }
   
   def getLocationFromDiplomacyUnit(diplomacyUnit: DiplomacyUnit): Location = {
     val dbSession = Session.create(conn, new RevisedPostgreSqlAdapter)
     using(dbSession) {
       diplomacyUnit.unitLocation.single
     }
   }
   
   def getAllActiveGames(): List[Game] = {
     val dbSession = Session.create(conn, new RevisedPostgreSqlAdapter)
     using(dbSession) {
       from(Jdip.games) ( (g: Game) =>
         where(g.gameStateID === GameState.ACTIVE)
         select(g)
       ).toList
     }
   }
   
   def getLocationFromLocationIDs(locationIDs: List[Int]): List[Location] = {
     val dbSession = Session.create(conn, new RevisedPostgreSqlAdapter)
     using(dbSession) {
       from(Jdip.locations)( (loc: Location) => 
         where(loc.id in locationIDs)
         select(loc)
       ).toList
     }
   }
}