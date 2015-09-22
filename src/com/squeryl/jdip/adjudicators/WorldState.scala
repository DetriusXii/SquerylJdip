package com.squeryl.jdip.adjudicators

import com.squeryl.jdip.tables._
import scalaz.effect._
import scalaz.effect.IO._

class WorldState private (provinces: List[Province], 
    combatList: List[(Province, IO[IORef[List[OrderState]]])],
    val convoySucceededList: List[OrderState],
    val convoyingArmiesList: List[OrderState]) {
  def this(provinces: List[Province], 
      combatList: List[(Province, IO[IORef[List[OrderState]]])]) = 
        this(provinces, combatList, Nil, Nil)
  def this(provinces: List[Province]) = 
    this(provinces, provinces.map((_, newIORef[List[OrderState]]({Nil}))))
  
  
  def addCombatUnitToProvince(province: Province, combatUnit: OrderState): IO[Unit] = 
    combatList.find(_._1.id == province.id).map(t =>
    	for ( ioRef <- t._2;
			_ <- ioRef.mod(combatUnit :: _)
    	) yield (())
    ).getOrElse(IO(()))
    
  def removeUnitFromProvince(province: Province, combatUnit: OrderState): IO[Unit] =
    combatList.find(_._1.id.compareTo(province.id) == 0).map(t =>
    	for (ioRef <- t._2;
    		_ <- ioRef.mod()
    	)
    )
    
  def getCombatListForProvince(provinceID: String): IO[List[OrderState]] = 
    combatList.find(_._1.id.compareTo(provinceID) == 0).map(_._2.flatMap(_.read)).getOrElse(IO({Nil}))

  def addConvoySucceeded(os: OrderState) = new WorldState(provinces, combatList, 
      os :: convoySucceededList, convoyingArmiesList)
  def addConvoyingArmy(os: OrderState) = 
    new WorldState(provinces, combatList, convoySucceededList, os :: convoyingArmiesList)
}