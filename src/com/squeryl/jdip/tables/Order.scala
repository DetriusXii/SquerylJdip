/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.squeryl.jdip.tables

import java.sql.Timestamp

case class Order(gameState: String, 
            gamePlayer: Int, 
            orderType: String,
            unitType: String,
            timestamp: Timestamp,
            srcLocation: String,
            dstLocation: Option[String],
            unitLocation: String) {
  def this() = this("", 0, "", "", new Timestamp(0L), "", Some(""), "")
} 
