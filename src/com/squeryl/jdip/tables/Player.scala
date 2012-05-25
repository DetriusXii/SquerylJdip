/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.squeryl.jdip.tables

import org.squeryl.KeyedEntity

class Player(val id: String, val password: String, val email: Option[String]) extends KeyedEntity[String] {
  def this() = this("", "", Some(""))
  def this(id: String, password: String) = this(id, password, Some(""))
}