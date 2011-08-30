/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.squeryl.jdip.tables

class Phase(val phase: String) extends KeyedEntity[String] {
  def this() = this("")
  
  val id = phase
}