package com.squeryl.jdip.creators

import com.squeryl.jdip.tables.Province

object ProvinceCreator {
	val PROVINCE_TAGNAME = "PROVINCE"
	val ADJACENCY_TAGNAME = "ADJACENCY"
	val UNIQUENAME_TAGNAME = "UNIQUENAME"
	val SHORTNAME_ATTRIBUTE = "shortname"
	val FULLNAME_ATTRIBUTE = "fullname"
	val TYPE_ATTRIBUTE = "type"
	val REFS_ATTRIBUTE = "refs"
	val NAME_ATTRIBUTE = "name"
	val LAND_TYPE = "mv"
	val ALL_COAST_TYPE = "xc"
  
	def isLandProvince(provinceNode: scala.xml.Node): Boolean = {
	  val adjacencyNodeSeq = provinceNode \\ ADJACENCY_TAGNAME
	  
	  adjacencyNodeSeq.exists(_.attribute(TYPE_ATTRIBUTE).map(_.toString) match {
	    case Some(LAND_TYPE) => true
	    case _ => false
	  })
	}
	  
	def getProvinces(adjacencyList: scala.xml.Elem): Iterable[Province] = {
	  val provinceNodeSeq: scala.xml.NodeSeq = adjacencyList \\ PROVINCE_TAGNAME
	  
	  provinceNodeSeq.map((provinceNode: scala.xml.Node) => {
	    for (shortname <- provinceNode.attribute(SHORTNAME_ATTRIBUTE).map(_.toString);
	    	 fullname <- provinceNode.attribute(FULLNAME_ATTRIBUTE).map(_.toString)
	    ) yield {
	      new Province(shortname, fullname, isLandProvince(provinceNode))
	    }
	  }).flatten
	}
}