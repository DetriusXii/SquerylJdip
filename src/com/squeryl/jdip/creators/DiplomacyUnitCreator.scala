package com.squeryl.jdip.creators
import scala.xml._
import com.squeryl.jdip.tables.DiplomacyUnit
import scalaz._
import com.squeryl.jdip.tables._
import java.io.File
import scalaz.OptionT._

object DiplomacyUnitCreator {
	val NAME_ATTRIBUTE = "name"
	val STANDARD_VARIANT_NAME = "Standard"
	val INITIALSTATE_TAGNAME = "INITIALSTATE"
	val PROVINCE_ATTRIBUTE = "province"
	val POWER_ATTRIBUTE = "power"
	val UNIT_ATTRIBUTE = "unit"
	val UNITCOAST_ATTRIBUTE = "unitcoast"
	val VARIANT_TAGNAME = "VARIANT"
	
	implicit def optionToOptionT[A](option: Option[A]) = 
	  optionT[Iterable].apply(option :: Nil)
	  
	  
	def getLocationForCoastAndUnitType(unitType: String, 
	    coastOption: Option[String], province: String, locations: Iterable[Location]): Option[Location] =
	  (unitType, coastOption) match {
	    case (_, Some(coast)) => locations.find((loc: Location) => 
	      loc.province.equalsIgnoreCase(province) && loc.coast.equalsIgnoreCase(coast))
	    case (UnitType.FLEET, _) => locations.find((loc: Location) =>
	      loc.province.equalsIgnoreCase(province) && loc.coast.equalsIgnoreCase(Coast.ALL_COAST))
	    case _ => locations.find((loc: Location) =>
	      loc.province.equalsIgnoreCase(province) && loc.coast.equalsIgnoreCase(Coast.NO_COAST))
	  }
	  
	def getDiplomacyUnits(gamePlayerEmpires: Iterable[GamePlayerEmpire],
						  empires: Iterable[Empire],
						  unitTypes: Iterable[UnitType],
						  locations: Iterable[Location],
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
	        	empire <- empires.find(_.alternateName.equalsIgnoreCase(power.toString));
	        	owner <- gamePlayerEmpires.find(gpe => gpe.empireName.equalsIgnoreCase(empire.id));
	        	correctedUnitType <- unitTypes.find(ut => ut.id.equalsIgnoreCase(unitType.toString));
	        	location <- getLocationForCoastAndUnitType(unitType.toString, 
	        	    initialStateElem.attribute(UNITCOAST_ATTRIBUTE).map(_.toString),
	        	    province.toString, locations)
	        ) yield {
	          new DiplomacyUnit(correctedUnitType.id, owner.id, location.id, unitNumber, gameTime.id)
	        }
	    })
	  }) match {
	    case Some(u) => u.flatten
	    case None => Nil
	  }
	}
}