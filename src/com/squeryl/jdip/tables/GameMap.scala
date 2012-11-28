package com.squeryl.jdip.tables
import org.squeryl.KeyedEntity
import org.squeryl.PrimitiveTypeMode._
import com.squeryl.jdip.schemas.Jdip

case class GameMap(gameID: String, 
    gameTimeID: Int, gameMap: BinaryType) extends KeyedEntity[Int] {
  val id = 0
  
  def this() = this("", 0, new BinaryType(0))
  
  lazy val game = Jdip.gmGameForeignKey.right(this)
  lazy val gameTime = Jdip.gmGameTimeForeignKey.right(this)
}