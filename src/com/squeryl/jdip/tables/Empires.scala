/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.squeryl.jdip.tables

import org.squeryl.KeyedEntity
import org.squeryl.PrimitiveTypeMode._

class Empires(
  val id: String, 
  val empireDescription: BinaryType,
  val startingFleetUnits: Int,
  val startingArmyUnits: Int) extends KeyedEntity[String] {
  def this() = this("", new BinaryType(0), 0, 0)
  def this(empireName: String) = this(empireName, new BinaryType(0), 0, 0)
  def this(empireName: String, empireDescription: String) = 
    this(empireDescription, empireDescription.trim.getBytes, 0, 0);
  def this(empireName: String, empireDescription: String, startingFleetUnits: Int, startingArmyUnits: Int) =
    this(empireName, empireDescription.trim.getBytes, startingFleetUnits, startingArmyUnits)
  
  def empireDescriptionString: String = new String(empireDescription)
}
