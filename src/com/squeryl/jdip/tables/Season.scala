/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.squeryl.jdip.tables

import org.squeryl.KeyedEntity

class Season(val season: String) extends KeyedEntity[String] {
  def this() = this("")
  
  val id = season
}
