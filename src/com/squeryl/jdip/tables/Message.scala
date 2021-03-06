/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.squeryl.jdip.tables

import org.squeryl.KeyedEntity
import org.squeryl.dsl._
import java.sql.Timestamp
import org.squeryl.PrimitiveTypeMode._



case class Message(senderId: Int, 
               receiverId: Int, 
               timestamp: Timestamp,
               message: String) extends KeyedEntity[CompositeKey3[Int, Int, java.sql.Timestamp]] {
  def this() = this(0, 0, new Timestamp(0L), "")
  
  def id = compositeKey(senderId, receiverId, timestamp)
}
