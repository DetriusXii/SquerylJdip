/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.squeryl.jdip.tables

import org.squeryl.KeyedEntity
import org.squeryl.PrimitiveTypeMode._
import scala.xml._

case class Empire(
  id: String,
  startingArmyUnits: Int,
  startingFleetUnits: Int,
  unitColour: String,
  provinceColour: String,
  armySVGElement: BinaryType,
  fleetSVGElement: BinaryType) extends KeyedEntity[String] {
  
  def this() = this("", 0, 0, "", "", new BinaryType(0), new BinaryType(0))
  def this(id: String) = this(id, 0, 0, "", "", new BinaryType(0), new BinaryType(0))
  def getArmySVGElementAsElem: Elem = XML.loadString(new String(armySVGElement))
  def getFleetSVGElementAsElem: Elem = XML.loadString(new String(armySVGElement))
}
