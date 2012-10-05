/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.squeryl.jdip.creators

import org.squeryl._
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.dsl.ast._
import com.squeryl.jdip.tables._
import scala.xml._
import com.squeryl.jdip.exceptions._

object EmpireCreator {
	private val ENGLAND_EMPIRE_NAME: String = "England"
	private val RUSSIA_EMPIRE_NAME: String = "Russia"
	private val ids: List[String] = "Austria" :: "England" :: "France" :: "Germany" :: "Italy" :: 
          "Russia" :: "Turkey" :: Nil
	private val unitColourClasses: List[String] = 
	  	"unitaustria" :: "unitengland" :: "unitfrance" :: 
		"unitgermany" :: "unititaly" :: "unitrussia" :: "unitturkey" :: Nil
	private val provinceColourClasses: List[String] = 
	  "austria" :: "england" :: "france" :: "germany" :: "italy" :: 
	  "russia" :: "turkey" :: Nil 
	
	private val armySymbol: Elem = <symbol id="Army" viewBox="0 0 23 15" overflow="visible">
            <g>
              <rect x="2" y="2" width="23" height="13" rx="4" stroke-width="1" class="symShadow" />
              <rect x="0" y="0" width="23" height="13" rx="4" class="symBorder" />
              <g class="symSilhouette">
                      <rect x="6" y="6" width="13" height="1"/>
                      <rect x="5" y="7" width="14" height="1"/>
                      <rect x="6" y="8" width="12" height="1"/>
                      <rect x="7" y="9" width="10" height="1"/>
                      <rect x="10" y="3" width="5" height="3"/>
                      <rect x="15" y="4.5" width="1" height="1.5"/>
                      <line x1="3" y1="4" x2="10" y2="4"/>
              </g>
            </g>	
    </symbol>
	
	private val fleetSymbol: Elem = 
		<symbol id="Fleet" viewBox="0 0 23 15" overflow="visible">
			<g>
			  <rect x="2" y="2" width="23" height="13" rx="4" 
					stroke-width="1" class="symShadow" />
			  <rect x="0" y="0" width="23" height="13" rx="4" class="symBorder" />
			  <g class="symSilhouette">
				<rect x="3" y="7" width="16.5" height="1"/>
				<rect x="4" y="8" width="15" height="1"/>
				<rect x="5" y="9" width="13.5" height="1"/>
				<rect x="13.5" y="6" width="2.75" height="1"/>
				<rect x="7" y="5"  width="4" height="2"/>
				<rect x="8.5" y="4"  width="1" height="1"/>
				<rect x="6" y="6" width="1" height="1"/>
			  </g>
			</g>	
		</symbol>
	
	@throws(classOf[IDElementNotFoundException])
	def getSVGTemplate(x: Elem)(fillColour: String): Elem = {
		val id = x.attribute("id") match {
		  case Some(x) => x.toString
		  case _ => throw new IDElementNotFoundException("ID attribute not found")
		}
		val anchor = "#" + id
		<svg xmlns="http://www.w3.org/2000/svg" 
			xmlns:xlink="http://www.w3.org/1999/xlink" viewBox="0 0 100 100">
			<defs>
			  {x}
			</defs>
		  <g>
			<use fill={fillColour} xlink:href={anchor} />
		  </g>
		</svg>
	} 
	
	private def getEmpires: List[Empire] = {
          var empireList: List[Empire] = Nil

          try {
            val getEmpireArmyElement: String => Elem = getSVGTemplate(armySymbol)
            val getEmpireFleetElement: String => Elem = getSVGTemplate(fleetSymbol)

            val cssClasses = unitColourClasses zip provinceColourClasses
            empireList = (ids zip cssClasses) map ((u: Tuple2[String, Tuple2[String, String]]) => {
              val id = u._1
              val unitColour = u._2._1
              val provinceColour = u._2._2
              
              val armyElement = getEmpireArmyElement(unitColour).toString.getBytes
              val fleetElement = getEmpireFleetElement(unitColour).toString.getBytes

              id match {
                case ENGLAND_EMPIRE_NAME => Empire(id, 1, 2, unitColour, provinceColour, 
                    armyElement, fleetElement) 
                case RUSSIA_EMPIRE_NAME => Empire(id, 2, 2, unitColour, provinceColour, 
                    armyElement, fleetElement)
                case _ => Empire(u._1, 1, 2, unitColour, provinceColour, armyElement, fleetElement)
              }
            })
          } catch {
            case idElemEx: IDElementNotFoundException => println(idElemEx.getMessage)
          }

          return empireList
	}
	
	lazy val empireList: List[Empire] = getEmpires
}