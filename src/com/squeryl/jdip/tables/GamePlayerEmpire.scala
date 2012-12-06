/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.squeryl.jdip.tables

import org.squeryl.KeyedEntity
import com.squeryl.jdip.schemas.Jdip

case class GamePlayerEmpire(gamePlayerKey: Int, empireName: String) extends 
	KeyedEntity[Int] {
  def this() = this(0, "")
  
  val id = 0
  
  lazy val ownedProvinces = Jdip.owpGamePlayerEmpireForeignKey.left(this)
  lazy val diplomacyUnits = Jdip.dpuOwnerForeignKey.left(this)
}
