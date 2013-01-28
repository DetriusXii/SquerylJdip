/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.squeryl.jdip.tables

import java.sql.Timestamp
import com.squeryl.jdip.schemas.Jdip
import org.squeryl.dsl.ManyToOne
import org.squeryl.KeyedEntity

case class Order(id: Int,
            orderType: String,
            timestamp: Timestamp,
            srcLocationIDOption: Option[Int],
            dstLocationIDOption: Option[Int]) extends KeyedEntity[Int] {
  def this() = this(0, "", new Timestamp(0L), Some(0), Some(0))
  
  lazy val srcLocation: ManyToOne[Location] =
    Jdip.srcLocationOrdersForeignKey.right(this)
  lazy val dstLocation: ManyToOne[Location] =
    Jdip.dstLocationOrdersForeignKey.right(this)
  lazy val diplomacyUnit: ManyToOne[DiplomacyUnit] =
    Jdip.dpuOrdersForeignKey.right(this)
} 
