package com.squeryl.jdip.creators
import scala.xml._
import com.squeryl.jdip.tables.DiplomacyUnit

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
	  
	def getDiplomacyUnits = {
	  val variantsXML = XML.load(VARIANTS_FILE_NAME)
	  
	  val variantNode = (variantsXML \\ VARIANT_TAGNAME).filter(_ match {
	    case u: Elem => u.attribute(NAME_ATTRIBUTE).toString.equals(STANDARD_VARIANT_NAME)
	    case _ => false
	  }).head
	  
	  val variantElem = variantNode match {
	    case u: Elem => u
	    case _ => throw new Exception("variantNode is not of type Elem")
	  }
	  
	  variantElem \\ INITIALSTATE_TAGNAME map (_ match {
	    case u: Elem => {
	      val province = u.attribute(PROVINCE_ATTRIBUTE)
	      val power = u.attribute(POWER_ATTRIBUTE)
	      val unitType = u.attribute(UNIT_ATTRIBUTE)
	      val unitCoast = u.attribute(UNITCOAST_ATTRIBUTE)
	      
	      new DiplomacyUnit(unitType, )
	    }
	    case _ => throw new Exception("Nodes are not of type Elem")
	  })
	}
}