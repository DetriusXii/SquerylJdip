package com.squeryl.jdip.queries
import com.squeryl.jdip.tables._
import org.squeryl._
import com.squeryl.jdip.adapters.RevisedPostgreSqlAdapter
import org.squeryl.PrimitiveTypeMode._
import com.squeryl.jdip.schemas.Jdip

class DeleteStatements(conn: () => java.sql.Connection) {
  def deleteOwnedProvincesForGame(game: Game): List[OwnedProvince] = {
    val session = new Session(conn(), new RevisedPostgreSqlAdapter)
    val dbQueries = new DBQueries(conn)
    
    val ownedProvincesForGame = dbQueries.getOwnedProvincesForGame(game)
    
    using(session) {
      ownedProvincesForGame.foreach(owpForGame =>
      	Jdip.ownedProvinces.deleteWhere(owp => owp.id === owpForGame.id)
      )
    }
    
    ownedProvincesForGame
  }
}