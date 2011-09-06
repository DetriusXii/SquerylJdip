/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.squeryl.jdip.tables

import org.squeryl.KeyedEntity
import org.squeryl.PrimitiveTypeMode._

class Games(val gameName: String, val gameFile: Option[BinaryType]) extends KeyedEntity[String] {
  def this() = this("", Some(new BinaryType(0)))
  
  def this(u: String) = this(u, Some(new BinaryType(0)))
  
  val id = gameName
  
} 
