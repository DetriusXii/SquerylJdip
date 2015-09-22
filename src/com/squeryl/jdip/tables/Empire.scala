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
  armySVGElement: Array[Byte],
  fleetSVGElement: Array[Byte],
  alternateName: String) extends KeyedEntity[String] {
  
  def this() = this("", 0, 0, "", "", new Array[Byte](0), new Array[Byte](0), "")
  def this(id: String) = this(id, 0, 0, "", "", new Array[Byte](0), new Array[Byte](0), id)
  def this(id: String, alternateName: String) =
    this(id, 0, 0, "", "", new Array[Byte](0), new Array[Byte](0), alternateName)
  def getArmySVGElementAsElem: Elem = XML.loadString(new String(armySVGElement))
  def getFleetSVGElementAsElem: Elem = XML.loadString(new String(armySVGElement))
}
