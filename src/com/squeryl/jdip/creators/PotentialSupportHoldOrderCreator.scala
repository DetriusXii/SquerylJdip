package com.squeryl.jdip.creators
import com.squeryl.jdip.queries.DBQueries
import com.squeryl.jdip.tables._

class PotentialSupportHoldOrderCreator(game: Game, dbQueries: DBQueries) {
  
  
  def createPotentialSupportHoldOrders(): List[PotentialSupportHoldOrder] = {
    val dpusForGame = 
      dbQueries.getDiplomacyUnitsForGameAtCurrentGameTime(game)
  }
}