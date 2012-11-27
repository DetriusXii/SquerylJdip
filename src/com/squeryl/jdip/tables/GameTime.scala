package com.squeryl.jdip.tables

import org.squeryl.KeyedEntity

case class GameTime(gameYear: Int, 
    gameSeason: String, 
    gamePhase: String) extends KeyedEntity[Int] {
	val id = 0
	def this() = this(0, "", "")
}

object GameTime {
  val MIN_GAME_YEAR = 1901;
  val MAX_GAME_YEAR = 1917;
  
  private val SEASON_LIST = Season.SPRING :: Season.FALL :: Nil
  private val SPRING_PHASE_LIST = Phase.MOVEMENT :: Phase.RETREAT :: Nil
  private val FALL_PHASE_LIST = Phase.MOVEMENT :: Phase.RETREAT :: Phase.BUILD :: Nil
  
  def getGameTimes: List[GameTime] = (MIN_GAME_YEAR until MAX_GAME_YEAR).flatMap((year: Int) => {
    SEASON_LIST.flatMap((season: SeasonCaseClass) => {
      season match {
        case Spring(seasonName: String) => SPRING_PHASE_LIST map ((phase: String) => {
          new GameTime(year, seasonName, phase)
        })
        case Fall(seasonName: String) => FALL_PHASE_LIST map ((phase: String) => {
          new GameTime(year, seasonName, phase)
        })
      }
    })
  }).toList
  
}