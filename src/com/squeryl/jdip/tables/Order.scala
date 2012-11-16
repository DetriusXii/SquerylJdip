/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.squeryl.jdip.tables

import java.sql.Timestamp

case class Order(
            gamePlayerEmpire: Int, 
            orderType: String,
            unitType: String,
            timestamp: Timestamp,
            srcLocation: Int,
            dstLocation: Option[Int],
            unitLocation: Int) {
  def this() = this(0, "", "", new Timestamp(0L), 0, Some(0), 0)
} 
