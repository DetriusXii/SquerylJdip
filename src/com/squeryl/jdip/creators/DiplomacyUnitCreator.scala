package com.squeryl.jdip.creators
import scala.xml._
import com.squeryl.jdip.tables.DiplomacyUnit
import scalaz._
import com.squeryl.jdip.tables.GamePlayerEmpire
import com.squeryl.jdip.tables.GameTime

object DiplomacyUnitCreator {
	val VARIANTS_FILE_NAME: String = "/home/detriusxiiuser/jdip/variants/stdVariants/variants.xml"
	val VARIANT_TAGNAME: String = "VARIANT"
	val NAME_ATTRIBUTE: String = "name"
	val STANDARD_VARIANT_NAME = "Standard"
	val INITIALSTATE_TAGNAME = "INITIALSTATE"
	val PROVINCE_ATTRIBUTE = "province"
	val POWER_ATTRIBUTE = "power"
	val UNIT_ATTRIBUTE = "unit"
	val UNITCOAST_ATTRIBUTE = "unitcoast"
	  
	def getDiplomacyUnits(gamePlayerEmpires: Iterable[GamePlayerEmpire], gameTime: GameTime): Iterable[DiplomacyUnit] = {
	  val variantsXML = XML.load(VARIANTS_FILE_NAME)
	  
	  val variantNode = (variantsXML \\ VARIANT_TAGNAME).filter(_ match {
	    case u: Elem => u.attribute(NAME_ATTRIBUTE).toString.equals(STANDARD_VARIANT_NAME)
	    case _ => false
	  }).head
	  
	  val variantElem = variantNode match {
	    case u: Elem => Some(u)
	    case _ => None
	  }
	  
	  
	  variantElem.flatMap((u: Elem) => {
	    val initialStateProjection = u \\ INITIALSTATE_TAGNAME
	    val unitNumberWithInitialStateProjection = 
	    	(0 until initialStateProjection.length) zip initialStateProjection
	    val diplomacyUnitListOption = unitNumberWithInitialStateProjection map (_ match {
	      case (unitNumber: Int, initialStateElem: Elem) => 
	        for (province <- u.attribute(PROVINCE_ATTRIBUTE);
				  power <- u.attribute(POWER_ATTRIBUTE);
				  unitType <- u.attribute(UNIT_ATTRIBUTE);
				  unitCoast <- u.attribute(UNITCOAST_ATTRIBUTE);
				  owner <- gamePlayerEmpires.find(gpe => gpe.empireName.equals(power.toString))
			) yield {
			  val location = u.attribute(UNITCOAST_ATTRIBUTE) match {
			    case Some(coast) => "%s-%s" format (province.toString, coast.toString)
			    case None => province.toString
			  }
			  new DiplomacyUnit(unitType.toString, owner.id, location, unitNumber, gameTime.id)
			}
	      case _ => None
	    })
	    Some(diplomacyUnitListOption.flatten)
	  }) match {
	    case Some (u) => u
	    case None => Nil
	  }
	}
}