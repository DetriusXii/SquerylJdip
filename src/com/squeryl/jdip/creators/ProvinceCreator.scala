package com.squeryl.jdip.creators

import scala.xml._
import com.squeryl.jdip.tables.Province

object ProvinceCreator {
	val JDIP_MAP_SVG_FILENAME: String = "/home/detriusxiiuser/jdip/variants/stdVariants/egdipmap.svg"
	val PROVINCE_TAGNAME =  "PROVINCE"
	val SUPPLY_CENTER_TAGNAME = "SUPPLY_CENTER"
	val NAME_ATTRIBUTE = "name"
	val BAD_XML_DATA_MESSAGE = "Province XML node data is not well formed"
	  
	def getProvinceList: Iterable[Province] = {
	  val jdipMapSVG = XML.load(JDIP_MAP_SVG_FILENAME)
	  
	  (jdipMapSVG \\ PROVINCE_TAGNAME) map ((provinceNode: Node) => {
	    val supplyCenterNodeExists = provinceNode.child.exists(_ match {
	      case elem: Elem => elem.label.equals(SUPPLY_CENTER_TAGNAME)
	      case _ => false
	    })
	    
	    val provinceElem = provinceNode match {
	      case elem: Elem => elem
	      case _ => throw new Exception(BAD_XML_DATA_MESSAGE)
	    }
	    
	    val provinceName = provinceElem.attribute(NAME_ATTRIBUTE) match {
	      case Some(name: Seq[_]) => name.toString
	      case None => throw new Exception(BAD_XML_DATA_MESSAGE)
	    }
	    
	    new Province(provinceName, supplyCenterNodeExists)
	  })
	}
}