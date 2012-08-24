package com.squeryl.jdip.creators
import scala.xml._
import com.squeryl.jdip.tables.DiplomacyUnit
import scalaz._
import com.squeryl.jdip.tables.GamePlayerEmpire
import com.squeryl.jdip.tables.GameTime
import com.squeryl.jdip.tables.UnitType
import java.io.File

object DiplomacyUnitCreator {
	val NAME_ATTRIBUTE = "name"
	val STANDARD_VARIANT_NAME = "Standard"
	val INITIALSTATE_TAGNAME = "INITIALSTATE"
	val PROVINCE_ATTRIBUTE = "province"
	val POWER_ATTRIBUTE = "power"
	val UNIT_ATTRIBUTE = "unit"
	val UNITCOAST_ATTRIBUTE = "unitcoast"
	val VARIANT_TAGNAME = "VARIANT"
	
	  
	def getDiplomacyUnits(gamePlayerEmpires: Iterable[GamePlayerEmpire],
						  unitTypes: Iterable[UnitType],
						  gameTime: GameTime,
						  variantsXML: Elem): Iterable[DiplomacyUnit] = {
	  
	  val variantNodeOption = (variantsXML \\ VARIANT_TAGNAME).find(_ match {
	    case u: Elem => u.attribute(NAME_ATTRIBUTE) match {
	      case Some(u) => u.toString.equals(STANDARD_VARIANT_NAME)
	      case _ => false
	    }
	    case _ => false
	  })
	  
	  variantNodeOption.map((variantNode: scala.xml.Node) => {
	    val initialStateProjection = variantNode \\INITIALSTATE_TAGNAME
	    val unitNumberWithInitialStateProjection = 
	      (0 until initialStateProjection.length) zip initialStateProjection
	    unitNumberWithInitialStateProjection map (_ match {
	      case (unitNumber: Int, initialStateElem: Elem) =>
	        for (province <- initialStateElem.attribute(PROVINCE_ATTRIBUTE);
	        	power <- initialStateElem.attribute(POWER_ATTRIBUTE);
	        	unitType <- initialStateElem.attribute(UNIT_ATTRIBUTE);
	        	owner <- gamePlayerEmpires.find(gpe => gpe.empireName.equalsIgnoreCase(power.toString));
	        	correctedUnitType <- unitTypes.find(ut => ut.id.equalsIgnoreCase(unitType.toString))
	        ) yield {
	          val location = initialStateElem.attribute(UNITCOAST_ATTRIBUTE) match {
	            case Some(coast) => "%s-%s" format (province.toString, coast.toString)
	            case None => province.toString
	          }
	          new DiplomacyUnit(correctedUnitType.id, owner.id, location, unitNumber, gameTime.id)
	        }
	    })
	  }) match {
	    case Some(u) => u.flatten
	    case None => Nil
	  }
	}
}