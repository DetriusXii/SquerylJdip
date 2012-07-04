/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.squeryl.jdip.tables

import org.squeryl.KeyedEntity

class Player(val id: String, 
             val password: String, 
             val email: Option[String],
             val wins: Int,
             val losses: Int) extends KeyedEntity[String] {
  def this() = this("", "", Some(""), 0, 0)
  def this(id: String, password: String) = this(id, password, Some(""), 0, 0)
}