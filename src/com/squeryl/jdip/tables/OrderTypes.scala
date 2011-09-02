/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.squeryl.jdip.tables

import org.squeryl.KeyedEntity

class OrderTypes(val orderType: String) extends KeyedEntity[String] {
  def this() = this("")
  
  val id = orderType
}
