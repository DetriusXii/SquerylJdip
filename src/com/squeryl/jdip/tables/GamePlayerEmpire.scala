/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.squeryl.jdip.tables

import org.squeryl.KeyedEntity

case class GamePlayerEmpire(gamePlayerKey: Int, empireName: String) extends KeyedEntity[Int] {
  def this() = this(0, "")
  
  val id = 0
}
