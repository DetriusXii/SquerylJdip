package com.squeryl.jdip.queries
import com.squeryl.jdip.tables._
import org.squeryl.Session
import com.squeryl.jdip.adapters.RevisedPostgreSqlAdapter
import org.squeryl.PrimitiveTypeMode._
import com.squeryl.jdip.schemas.Jdip
import org.squeryl.Query

object DBQueries {
  lazy val locations: List[Location] = 
    transaction { Jdip.locations.allRows.toList }
  
  lazy val uniqueProvinceNames: List[UniqueProvinceName] = 
    transaction { Jdip.uniqueProvinceNames.allRows.toList }
    
  lazy val empires: List[Empire] = 
    transaction { Jdip.empires.allRows.toList }
  
  lazy val adjacencies: List[Adjacency] =
    transaction {Jdip.adjacencies.allRows.toList }
  
  lazy val provinces: List[Province] =
    transaction { Jdip.provinces.allRows.toList }
    
  lazy val orderTypeUnitTypes: List[OrderTypeUnitType] = 
  	transaction { Jdip.orderTypeUnitTypes.allRows.toList }

  lazy val orderTypes: List[OrderType] = 
    transaction { Jdip.orderTypes.allRows.toList }
  
  lazy val gameTimes: List[GameTime] =
    transaction { Jdip.gameTimes.allRows.toList }

  def getOwnedProvincesForGame(game: Game): List[OwnedProvince] = 
  	transaction {from(Jdip.ownedProvinces) (owp =>
        where(owp.gamePlayerEmpireID in gamePlayerEmpireQueryForGame(game) and 
          owp.gameTimeID === game.gameTimeID
        ) 
        select(owp)
      ).toList}
  
  def getGame(gameName: String): Option[Game] = 
    transaction {Jdip.games.lookup(gameName)}
    
  def getGamePlayerEmpire(gamePlayerEmpireID: Int): Option[GamePlayerEmpire] = 
  	transaction { Jdip.gamePlayerEmpires.lookup(gamePlayerEmpireID) }
    
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
  	transaction { from(Jdip.diplomacyUnits)(dpu => (
	    where((dpu.unitType === UnitType.ARMY) and 
	        (dpu.gamePlayerEmpireID in gamePlayerEmpireQueryForGame(game)) and
	        (dpu.gameTimeID === game.gameTimeID)) 
	    select(dpu)
	  )).toList }
  
  def getAllFleetUnitsForGame(game: Game): List[DiplomacyUnit] = 
  	transaction {from(Jdip.diplomacyUnits)(dpu => (
        where((dpu.unitType === UnitType.FLEET) and 
    	  (dpu.gamePlayerEmpireID in gamePlayerEmpireQueryForGame(game)) and
    	  (dpu.gameTimeID === game.gameTimeID)
        ) 
        select(dpu)
      )).toList
    }
  
  def getDiplomacyUnitsForGameAtCurrentGameTime(game: Game): List[DiplomacyUnit] =
	transaction {from(Jdip.diplomacyUnits)(dpu => 
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
  	transaction { from(Jdip.gamePlayerEmpires) (gpe =>
        where(gpe.id in gamePlayerEmpireQueryForGame(game: Game))
        select(gpe)
      ).toList }
  
  def getGameForGamePlayerEmpireID(gamePlayerEmpireID: Int): Option[Game] = 
  	transaction { 
	  from(Jdip.games, Jdip.gamePlayers, Jdip.gamePlayerEmpires)((g, gp, gpe) => 
	  	where((gpe.id === gamePlayerEmpireID) and
    	  (gpe.gamePlayerKey === gp.id) and
          (g.id === gp.gameName)
        )
        select(g)
      ).headOption }
  
  def getPlayerForGamePlayerEmpire(
      gamePlayerEmpire: GamePlayerEmpire): Option[Player] =
  transaction {
    from(Jdip.players, Jdip.gamePlayers, Jdip.gamePlayerEmpires)(
        (p, gp, gpe) => 
          where((gpe.id === gamePlayerEmpire.id) and
              (gpe.gamePlayerKey === gp.id) and
              (p.id === gp.playerName))
          select(p)
        ).headOption
  }
  
  def getGameForGamePlayerEmpire(
      gamePlayerEmpire: GamePlayerEmpire): Option[Game] = 
    transaction { 
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
	transaction { 
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
  	transaction { from(Jdip.players)(select(_)).toList }
  
  def getPlayerFromPlayerName(playerName: String): Option[Player] = 
  	transaction { from(Jdip.players)(pl => 
        where(pl.id === playerName)
        select(pl)
      ).headOption }

  def getGamePlayerEmpire(gameName: String, playerName: String): 
	  Option[GamePlayerEmpire] =  
	transaction {from(Jdip.gamePlayerEmpires) (gpe =>
        where(gpe.gamePlayerKey in from(Jdip.gamePlayers) (gp => 
          where(gp.gameName === gameName and gp.playerName === playerName)
          select(gp.id)))
        select(gpe)
      ).toList.headOption }
  
  def getGamesForUser(username: String): List[Game] = 
  	transaction {from(Jdip.games) (g =>
        where(g.id in from(Jdip.gamePlayers) (gp => 
          where (gp.playerName like username)
          select(gp.gameName)
          ))
        select(g)
      ).toList }
    
   def getGameTimesForGameTimeIDs(gameTimeIDs: List[Int]): List[GameTime] = 
   	 gameTimeIDs.map((gameTimeID: Int) => 
   	   gameTimes.find(_.id == gameTimeID)
   	 ).flatten
   
   def getLocationForDiplomacyUnit(diplomacyUnit: DiplomacyUnit): Location = 
     transaction { diplomacyUnit.unitLocation.single }
   
   def getAllActiveGames(): List[Game] = 
     transaction { from(Jdip.games) ( (g: Game) =>
         where(g.gameStateID === GameState.ACTIVE)
         select(g)
       ).toList }
   
   def getLocationForLocationIDs(locationIDs: List[Int]): List[Location] = 
   	 locationIDs.map((locationID: Int) => {
   	   locations.find(_.id == locationID)
   	 }).flatten
   
   def getGameMapForGameAtCurrentTime(game: Game): Option[GameMap] = 
     transaction { from(Jdip.gameMaps)((gm: GameMap) => 
       	 where(gm.gameID === game.id and gm.gameTimeID === game.gameTimeID)
       	 select(gm)
       ).headOption }
   
   def getPotentialMoveOrdersForGamePlayerEmpireAtCurrentTime(gpe: GamePlayerEmpire): 
     List[PotentialMoveOrder] = 
       transaction {
         from(gpe.diplomacyUnits, 
        	  Jdip.games,
        	  Jdip.gamePlayers,
              Jdip.potentialMoveOrders) ((dpu, g, gp, pmo) => 
           where(
             g.gameTimeID ===  dpu.gameTimeID and
             g.id === gp.gameName and
             gp.id === gpe.gamePlayerKey and
             dpu.id === pmo.diplomacyUnitID    
           )
           select (pmo)
         ).toList
   	  }
   
   def getDiplomacyUnitsWithMoveOrderForGameAtCurrentTime(game: Game): 
	   List[DiplomacyUnit] = transaction {
     from(Jdip.gamePlayers,
         Jdip.gamePlayerEmpires, 
         Jdip.orders,
         Jdip.diplomacyUnits)((gp, gpe, o, dpu) => 
     	where(
     	    gp.gameName === game.id and
     		gpe.gamePlayerKey === gp.id and
     		o.orderType === OrderType.MOVE and
     		o.id === dpu.id and
     		game.gameTimeID === dpu.gameTimeID
     	)
     	select(dpu)
     ).toList
   }
   
   def getPotentialSupportHoldOrdersForGamePlayerEmpire(gpe: GamePlayerEmpire):
     List[PotentialSupportHoldOrder] =
       transaction {
         from(gpe.diplomacyUnits,
             Jdip.potentialSupportHoldOrders) ((dpu, psho) =>
           where(
             dpu.id === psho.diplomacyUnitID    
           )
           select(psho)
         ).toList
       }
   
   def getPotentialSupportMoveOrdersForGamePlayerEmpire(gpe: GamePlayerEmpire):
     List[PotentialSupportMoveOrder] =
       transaction {
         from(Jdip.potentialSupportMoveOrders) (psmo =>
           where(psmo.diplomacyUnitID in from(gpe.diplomacyUnits)(dpu =>
               select(dpu.id)))
           select(psmo)
         ).toList
       }
   
   def getPotentialConvoyOrdersForGamePlayerEmpire(gpe: GamePlayerEmpire):
     List[PotentialConvoyOrder] =
       transaction {
         from(Jdip.potentialConvoyOrders) (pco =>
           where(pco.diplomacyUnitID in from(gpe.diplomacyUnits)(dpu =>
               select(dpu.id)  
           ))
           select(pco)
         ).toList
       }
   
   def getOrderForDiplomacyUnit(dpu: DiplomacyUnit): Option[Order] = transaction {
     from(Jdip.orders)(o => 
       where(o.id === dpu.id)
       select(o)
     ).headOption
   }
   
   def getProvinceFromOwnedProvince(owp: OwnedProvince): Option[Province] = 
     transaction {
	   owp.province.headOption
   }
   
   def getActiveGames(): List[Game] = transaction {
     from(Jdip.games)(g => 
     	where(g.gameStateID === GameState.ACTIVE)
     	select(g)
     ).toList
   }
   
   def getOrdersForDiplomacyUnits(dpus: List[DiplomacyUnit]): List[Order] = 
     transaction {
	   from(Jdip.orders)(o => 
	     where(o.id in dpus.map(_.id))
	     select(o)
	   ).toList
   	 }
}