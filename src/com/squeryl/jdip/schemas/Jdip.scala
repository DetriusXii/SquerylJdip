/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.squeryl.jdip.schemas

import com.squeryl.jdip.tables._
import org.squeryl._
import org.squeryl.PrimitiveTypeMode._

object Jdip extends PostgreSchema("jdip") {
   val players = table[Player]("players", schemaName)
   val phases = table[Phase]("phases", schemaName)
   val seasons = table[Season]("seasons", schemaName)
   val gameTimes = table[GameTime]("game_times", schemaName)
   val games = table[Game]("games", schemaName)
   val orderTypes = table[OrderType]("order_types", schemaName)
   val orders = table[Order]("orders", schemaName)
   val empires = table[Empire]("empires", schemaName)
   val unitTypes = table[UnitType]("unit_types", schemaName)
   val gameStates = table[GameState]("game_states", schemaName)
   val messages = table[Message]("messages", schemaName)
   val provinces = table[Province]("provinces", schemaName)
   val coasts = table[Coast]("coasts", schemaName)
   val uniqueProvinceNames = 
     table[UniqueProvinceName]("unique_province_names", schemaName)
   val diplomacyUnits = table[DiplomacyUnit]("diplomacy_units", schemaName)
   val ownedProvinces = table[OwnedProvince]("owned_provinces", schemaName)
   val potentialSupportMoveOrders =
     table[PotentialSupportMoveOrder]("potential_support_move_orders", schemaName)
   val potentialConvoyOrders =
     table[PotentialConvoyOrder]("potential_convoy_orders", schemaName)
   val gameMaps =
     table[GameMap]("game_maps", schemaName)
   
  val gamePlayers = 
     manyToManyRelation(games, players, "game_players", schemaName).
      via[GamePlayer]((g,p,gp) => 
         (g.id === gp.gameName, p.id === gp.playerName)
       ) 
       
  val gamePlayerEmpires = manyToManyRelation(gamePlayers, empires, "game_player_empires", schemaName).
    via[GamePlayerEmpire]((gamePlayers, empires, gpe) => 
      (gamePlayers.id === gpe.gamePlayerKey, empires.id === gpe.empireName)
    )
    
  val orderTypeUnitTypes = manyToManyRelation(orderTypes, unitTypes, "order_type_unit_types", schemaName).
  	via[OrderTypeUnitType]((ot, ut, otut) =>
  		(ot.id === otut.orderType, ut.id === otut.unitType)
  	)
  
  val locations	= manyToManyRelation(provinces, coasts, "locations", schemaName).
  	via[Location]((pr, co, loc) => (pr.id === loc.province, co.id === loc.coast))
  	
  val adjacencies = manyToManyRelation(locations, locations, "adjacencies", schemaName).via[Adjacency](
      (l1, l2, adjacency) => (l1.id === adjacency.srcLocation, l2.id === adjacency.dstLocation)
  )	
  	
  val potentialMoveOrders = 
    manyToManyRelation(diplomacyUnits, 
      locations, 
      "potential_move_orders", 
      schemaName).via[PotentialMoveOrder]((dpu, loc, pmo) => 
          (dpu.id === pmo.diplomacyUnitID, loc.id === pmo.moveLocationID))
          
  
  val potentialSupportHoldOrders =
    manyToManyRelation(diplomacyUnits, 
      locations,
      "potential_support_hold_orders", 
      schemaName).via[PotentialSupportHoldOrder]((dpu, loc, psho) => 
      	(dpu.id === psho.diplomacyUnitID, loc.id === psho.supportHoldLocationID)
      )
  
  val gtSeasonForeignKey = oneToManyRelation(seasons, gameTimes).via((s, gt) => s.id === gt.gameSeason)
  val gtPhaseForeignKey = oneToManyRelation(phases, gameTimes).via((p, gt) => p.id === gt.gamePhase)
  
  val gamesGameStateForeignKey = oneToManyRelation(gameStates, games).via((gs, g) => {
       gs.id === g.gameState
    })
  val gamesGameTimeForeignKey = oneToManyRelation(gameTimes, games).via((gt, g) => {
    gt.id === g.gameTime
  })
  
  val senderMessageForeignKey = 
    oneToManyRelation(gamePlayerEmpires, messages).via((gpr, m) =>
      gpr.id === m.senderId
    )
  val receiverMessageForeignKey =
    oneToManyRelation(gamePlayerEmpires, messages).via((gpr, m) =>
      gpr.id === m.receiverId
    )
 
  val phaseOrderTypesForeignKey = oneToManyRelation(phases, orderTypes).via((ph, ot) => 
  		ph.id === ot.phase
  )  
    
  val gamePlayerEmpiresOrdersForeignKey = 
    oneToManyRelation(gamePlayerEmpires, orders).via((gpr, o) => {
        gpr.id === o.gamePlayerEmpireID
    })
  val orderTypeOrdersForeignKey = oneToManyRelation(orderTypes, orders).via((ot, o) => 
        ot.id === o.orderType
    )
  val unitTypesOrdersForeignKey = oneToManyRelation(unitTypes, orders).via((ut, o) => 
        ut.id === o.unitType
    )
  val srcLocationOrdersForeignKey = oneToManyRelation(locations, orders).via((loc, o) => 
    loc.id === o.srcLocationID
  )
  val dstLocationOrdersForeignKey = oneToManyRelation(locations, orders).via((loc, o) =>
  	loc.id === o.dstLocationIDOption
  )
  val unitLocationOrdersForeignKey = oneToManyRelation(locations, orders).via((loc, o) => 
  	loc.id === o.unitLocationID
  )
   
  val uniProvinceForeignKey = oneToManyRelation(provinces, uniqueProvinceNames).via((pr, upn) =>
  	pr.id === upn.provinceName
  )  
    
  val dpuOwnerForeignKey = oneToManyRelation(gamePlayerEmpires, diplomacyUnits).via((gpe, dpu) => 
    gpe.id === dpu.gamePlayerEmpireID
  )
  val dpuLocationForeignKey = oneToManyRelation(locations, diplomacyUnits).via((loc, dpu) => 
    loc.id === dpu.unitLocationID
  )
  val dpuUnitTypeForeignKey = oneToManyRelation(unitTypes, diplomacyUnits).via((ut, dpu) => 
    ut.id === dpu.unitType
  )
  val dpuGameTimeForeignKey = oneToManyRelation(gameTimes, diplomacyUnits).via((gt, dpu) =>
  	gt.id === dpu.gameTimeID
  )

  val owpProvinceForeignKey = oneToManyRelation(provinces, ownedProvinces).via((pr, owp) =>
  	pr.id === owp.provinceID
  )
  val owpGamePlayerEmpireForeignKey = oneToManyRelation(gamePlayerEmpires, ownedProvinces).via((gpe, owp) =>
  	gpe.id === owp.gamePlayerEmpireID
  )
  val owpGameTimeForeignKey = oneToManyRelation(gameTimes, ownedProvinces).via((gt, owp) => 
    gt.id === owp.gameTimeID
  )
  
  val psmoDiplomacyUnitForeignKey = 
    oneToManyRelation(diplomacyUnits, potentialSupportMoveOrders).
    via((dpu, psmo) =>
      dpu.id === psmo.diplomacyUnitID
    )
  val psmoSourceLocationForeignKey =
    oneToManyRelation(locations, potentialSupportMoveOrders).
    via((loc, psmo) =>
      loc.id === psmo.supportMoveSourceLocationID
    )
  val psmoTargetLocationForeignKey =
    oneToManyRelation(locations, potentialSupportMoveOrders).
    via((loc, psmo) =>
      loc.id === psmo.supportMoveTargetLocationID  
    )
  
  val pcoDiplomacyUnitForeignKey =
    oneToManyRelation(diplomacyUnits, potentialConvoyOrders).
    via((dpu, pco) =>
      dpu.id === pco.diplomacyUnitID
    )
  val pcoSourceLocationForeignKey =
    oneToManyRelation(locations, potentialConvoyOrders).
    via((loc, pco) =>
      loc.id === pco.convoySourceLocationID  
    )
  val pcoTargetLocationForeignKey =
    oneToManyRelation(locations, potentialConvoyOrders).
    via((loc, pco) =>
      loc.id === pco.convoyTargetLocationID
    )
    
  val gmGameForeignKey = oneToManyRelation(games, gameMaps).
    via((g, gm) => g.id === gm.gameID)
  val gmGameTimeForeignKey = oneToManyRelation(gameTimes, gameMaps).
    via((gt, gm) => gt.id === gm.gameTimeID)
    
  on(gameTimes)(gt => declare(
    gt.id is(autoIncremented)    
  ))
    
  on(potentialSupportMoveOrders)(psmo => declare(
    psmo.id is(autoIncremented)
  ))
  
  on(potentialConvoyOrders)(pco => declare(
    pco.id is(autoIncremented)    
  ))
  
  on(diplomacyUnits)(dpu => declare(
		  columns(dpu.gamePlayerEmpireID, dpu.unitNumber, dpu.gameTimeID) are(unique)
  ))
  
  on(locations)((loc: Location) => declare(
		  columns(loc.province, loc.coast) are(unique)
  ))
}
