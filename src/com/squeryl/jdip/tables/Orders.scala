/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.squeryl.jdip.tables

import java.sql.Timestamp

class Orders(val gameStateId: Long, 
             val gamePlayerId: Int, 
             val orderType: String,
             val unitType: String,
             val timestamp: Timestamp,
             val srcLocation: String,
             val dstLocation: Option[String],
             val unitLocation: String) {
  def this() = this(0L, 0, "", "", new Timestamp(0L), "", None, "")
} 
