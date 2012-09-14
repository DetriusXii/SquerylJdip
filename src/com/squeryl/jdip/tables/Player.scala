/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.squeryl.jdip.tables

import org.squeryl.KeyedEntity

case class Player(id: String, 
             password: String, 
             email: Option[String],
             wins: Int,
             losses: Int) extends KeyedEntity[String] {
  def this() = this("", "", Some(""), 0, 0)
  def this(id: String, password: String) = this(id, password, Some(""), 0, 0)
}