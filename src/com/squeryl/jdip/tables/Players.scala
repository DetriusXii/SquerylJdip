/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.squeryl.jdip.tables

import org.squeryl.KeyedEntity
import org.squeryl.IndirectKeyedEntity

class Players(val id: String) extends KeyedEntity[String] {
  def this() = this("")
  
}