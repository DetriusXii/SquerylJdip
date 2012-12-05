package com.squeryl.jdip.queries
import com.squeryl.jdip.tables._
import org.squeryl.Session
import com.squeryl.jdip.adapters.RevisedPostgreSqlAdapter
import org.squeryl.PrimitiveTypeMode._
import com.squeryl.jdip.schemas.Jdip
import org.squeryl.Query

class DBQueries(conn: () => java.sql.Connection) {
  private def manageTransaction[A](codeBlock: => A): A  = {
    val connection = conn()
    val dbSession = Session.create(connection, new RevisedPostgreSqlAdapter)
    val returnValue = using(dbSession) {
      codeBlock
    }
    connection.close()
    returnValue
  }
  
  lazy val locations: List[Location] = 
    manageTransaction { Jdip.locations.toList }
  
  lazy val uniqueProvinceNames: List[UniqueProvinceName] = 
    manageTransaction { Jdip.uniqueProvinceNames.toList }
    
  lazy val empires: List[Empire] = 
    manageTransaction { Jdip.empires.toList }
  
  lazy val adjacencies: List[Adjacency] =
    manageTransaction {Jdip.adjacencies.toList }
  
  lazy val provinces: List[Province] =
    manageTransaction { Jdip.provinces.toList }
    
  lazy val orderTypeUnitTypes: List[OrderTypeUnitType] = 
  	manageTransaction { Jdip.orderTypeUnitTypes.toList }

  lazy val orderTypes: List[OrderType] = 
    manageTransaction { Jdip.orderTypes.toList }

  def getOwnedProvincesForGame(game: Game): List[OwnedProvince] = 
  	manageTransaction {from(Jdip.ownedProvinces) (owp =>
        where(owp.gamePlayerEmpireID in gamePlayerEmpireQueryForGame(game) and 
          owp.gameTimeID === game.gameTimeID
        ) 
        select(owp)
      ).toList}
  
  def getGame(gameName: String): Option[Game] = 
    manageTransaction {Jdip.games.lookup(gameName)}
    
  def getGamePlayerEmpire(gamePlayerEmpireID: Int): Option[GamePlayerEmpire] = 
  	manageTransaction { Jdip.gamePlayerEmpires.lookup(gamePlayerEmpireID) }
    
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
  
  def getAllLandUnitsForGame(game: Game): List[DiplomacyUnit] = 
  	manageTransaction { from(Jdip.diplomacyUnits)(dpu => (
	    where((dpu.unitType === UnitType.ARMY) and 
	        (dpu.gamePlayerEmpireID in gamePlayerEmpireQueryForGame(game)) and
	        (dpu.gameTimeID === game.gameTimeID)) 
	    select(dpu)
	  )).toList }
  
  def getAllFleetUnitsForGame(game: Game): List[DiplomacyUnit] = 
  	manageTransaction {from(Jdip.diplomacyUnits)(dpu => (
        where((dpu.unitType === UnitType.FLEET) and 
    	  (dpu.gamePlayerEmpireID in gamePlayerEmpireQueryForGame(game)) and
    	  (dpu.gameTimeID === game.gameTimeID)
        ) 
        select(dpu)
      )).toList
    }
  
  def getDiplomacyUnitsForGameAtCurrentGameTime(game: Game): List[DiplomacyUnit] =
	manageTransaction {from(Jdip.diplomacyUnits)(dpu => 
	      where((dpu.gamePlayerEmpireID in gamePlayerEmpireQueryForGame(game))
	      	and (dpu.gameTimeID === game.gameTimeID))
	      select(dpu)
	    ).toList }
  
  private def gamePlayerEmpireQueryForGame(game: Game): Query[Int] =
    from(Jdip.gamePlayerEmpires, Jdip.gamePlayers)((gpe, gp) => 
    	where(gpe.gamePlayerKey === gp.id and gp.gameName === game.id)
    	select(gpe.id)
    )
  
  def getGamePlayerEmpiresForGame(game: Game): List[GamePlayerEmpire] = 
  	manageTransaction { from(Jdip.gamePlayerEmpires) (gpe =>
        where(gpe.id in gamePlayerEmpireQueryForGame(game: Game))
        select(gpe)
      ).toList }
  
  def getGameForGamePlayerEmpireID(gamePlayerEmpireID: Int): Option[Game] = 
  	manageTransaction { 
	  from(Jdip.games, Jdip.gamePlayers, Jdip.gamePlayerEmpires)((g, gp, gpe) => 
	  	where((gpe.id === gamePlayerEmpireID) and
    	  (gpe.gamePlayerKey === gp.id) and
          (g.id === gp.gameName)
        )
        select(g)
      ).headOption }
    
  def getGameForGamePlayerEmpire(
      gamePlayerEmpire: GamePlayerEmpire): Option[Game] = 
    manageTransaction { 
	  	from(Jdip.games, 
	  		Jdip.gamePlayers, Jdip.gamePlayerEmpires)((g, gp, gpe) => 
	  		where((gpe.id === gamePlayerEmpire.id) and
      			(gpe.gamePlayerKey === gp.id) and
      			(g.id === gp.gameName)
	  		)
	  		select(g)
	  	).headOption }
 
  
  def getDiplomacyUnitsForGamePlayerEmpire(gamePlayerEmpire: GamePlayerEmpire):
	  List[DiplomacyUnit] = 
	manageTransaction { 
	  from(Jdip.games, 
	    Jdip.gamePlayers, Jdip.diplomacyUnits) ((g, gp, dpu) => 
	    where((gp.id === gamePlayerEmpire.gamePlayerKey) and
	    		(g.id === gp.gameName) and
	    		(dpu.gamePlayerEmpireID === gamePlayerEmpire.id) and
	    		(dpu.gameTimeID === g.gameTimeID)
	    )
	  	select(dpu)
	  ).toList }
	

  def getPlayers: List[Player] = 
  	manageTransaction { from(Jdip.players)(select(_)).toList }
  
  def getPlayerFromPlayerName(playerName: String): Option[Player] = 
  	manageTransaction { from(Jdip.players)(pl => 
        where(pl.id === playerName)
        select(pl)
      ).headOption }

  def getGamePlayerEmpire(gameName: String, playerName: String): 
	  Option[GamePlayerEmpire] =  
	manageTransaction {from(Jdip.gamePlayerEmpires) (gpe =>
        where(gpe.gamePlayerKey in from(Jdip.gamePlayers) (gp => 
          where(gp.gameName === gameName and gp.playerName === playerName)
          select(gp.id)))
        select(gpe)
      ).toList.headOption }
  
  def getGamesForUser(username: String): List[Game] = 
  	manageTransaction {from(Jdip.games) (g =>
        where(g.id in from(Jdip.gamePlayers) (gp => 
          where (gp.playerName like username)
          select(gp.gameName)
          ))
        select(g)
      ).toList }
    
   def getGameTimesForGames(gameTimeIDs: List[Int]): List[GameTime] = 
   	 manageTransaction {from(Jdip.gameTimes) (gt =>
         where(gt.id in gameTimeIDs)
         select(gt)
       ).toList }
   
   def getLocationForDiplomacyUnit(diplomacyUnit: DiplomacyUnit): Location = 
     manageTransaction { diplomacyUnit.unitLocation.single }
   
   def getAllActiveGames(): List[Game] = 
     manageTransaction { from(Jdip.games) ( (g: Game) =>
         where(g.gameStateID === GameState.ACTIVE)
         select(g)
       ).toList }
   
   def getLocationFromLocationIDs(locationIDs: List[Int]): List[Location] = 
   	 manageTransaction { from(Jdip.locations)( (loc: Location) => 
         where(loc.id in locationIDs)
         select(loc)
       ).toList }
   
   def getGameMapForGameAtCurrentTime(game: Game): Option[GameMap] = 
     manageTransaction { from(Jdip.gameMaps)((gm: GameMap) => 
       	 where(gm.gameID === game.id and gm.gameTimeID === game.gameTimeID)
       	 select(gm)
       ).firstOption }
   
   def getPotentialMoveOrdersForGamePlayerEmpire(gpe: GamePlayerEmpire): 
     List[PotentialMoveOrder] = 
       manageTransaction {
         from(Jdip.diplomacyUnits, Jdip.potentialMoveOrders) ((dpu, pmo) => 
           where(dpu.gamePlayerEmpireID === gpe.id and
             dpu.id === pmo.diplomacyUnitID    
           )
           select (pmo)
         ).toList
   	  }
}