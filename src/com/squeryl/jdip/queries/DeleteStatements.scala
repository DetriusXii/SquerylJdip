package com.squeryl.jdip.queries
import com.squeryl.jdip.tables._
import org.squeryl._
import com.squeryl.jdip.adapters.RevisedPostgreSqlAdapter
import org.squeryl.PrimitiveTypeMode._
import com.squeryl.jdip.schemas.Jdip

object DeleteStatements {
  def deleteOwnedProvincesForGame(game: Game): List[OwnedProvince] = {
    val ownedProvincesForGame = DBQueries.getOwnedProvincesForGame(game)
    
    transaction {
      ownedProvincesForGame.foreach(owpForGame =>
      	Jdip.ownedProvinces.deleteWhere(owp => owp.id === owpForGame.id)
      )
    }
    
    ownedProvincesForGame
  }
}