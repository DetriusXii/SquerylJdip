/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.squeryl.jdip.tables

import java.sql.Timestamp
import com.squeryl.jdip.schemas.Jdip
import org.squeryl.dsl.ManyToOne

case class Order(
            gamePlayerEmpireID: Int, 
            orderType: String,
            unitType: String,
            timestamp: Timestamp,
            srcLocationID: Int,
            dstLocationIDOption: Option[Int],
            unitLocationID: Int) {
  def this() = this(0, "", "", new Timestamp(0L), 0, Some(0), 0)
  
  lazy val gamePlayerEmpire: ManyToOne[GamePlayerEmpire] =
    Jdip.gamePlayerEmpiresOrdersForeignKey.right(this)
  lazy val srcLocation: ManyToOne[Location] =
    Jdip.srcLocationOrdersForeignKey.right(this)
  lazy val dstLocation: ManyToOne[Location] =
    Jdip.dstLocationOrdersForeignKey.right(this)
  lazy val unitLocation: ManyToOne[Location] =
    Jdip.unitLocationOrdersForeignKey.right(this)
} 
