package com.squeryl.jdip.creators

import com.squeryl.jdip.tables.Adjacency
import com.squeryl.jdip.tables.Province
import scala.xml._
import scalaz._
import com.squeryl.jdip.tables.UnitType

object AdjacencyCreator {
	val PROVINCE_TAGNAME = "PROVINCE"
	val ADJACENCY_TAGNAME = "ADJACENCY"
	val UNIQUENAME_TAGNAME = "UNIQUENAME"
	val SHORTNAME_ATTRIBUTE = "shortname"  
	val TYPE_ATTRIBUTE = "type"
	val REFS_ATTRIBUTE = "refs"
	val NAME_ATTRIBUTE = "name"
	val LAND_TYPE = "mv"
	val ALL_COAST_TYPE = "xc"
	  
	def getAdjacencyNodeSeqOptionT(provinceNode: scala.xml.Node): OptionT[Seq, scala.xml.Node] = {
	  val optionTTrait = new OptionTs {}
	  val adjacenciesNodeSeq = provinceNode \\ ADJACENCY_TAGNAME
	  
	  optionTTrait.optionT[Seq].apply(adjacenciesNodeSeq.map(Some(_)))
	}
	
	def getNeighbours(adjacencyNode: scala.xml.Node): OptionT[Seq, String] = {
	  val optionTTrait = new OptionTs {}
	  val refs = adjacencyNode.attribute(REFS_ATTRIBUTE) match {
	    case Some(u) => u.toString.split(" ").toSeq
	    case None => Nil
	  }
	  optionTTrait.optionT[Seq].apply(refs.map(Some(_)))
	}
	
	def getShortAndUniqueNames(provinceNode: scala.xml.Node): Iterable[String] = {
	  val uniqueNameNodeSeq = provinceNode \\ UNIQUENAME_TAGNAME
	  
	  val shortNameOption = provinceNode.attribute(SHORTNAME_ATTRIBUTE).map(_.toString)
	  
	  val allNames = shortNameOption :: 
		  (uniqueNameNodeSeq.map(_.attribute(NAME_ATTRIBUTE).map(_.toString)).toList)
	  allNames.flatten
	}
	
	def getShortNameToProvinceNameMap(provincesNodeSeq: NodeSeq, 
	    provinces: Iterable[Province]): Map[String, String] = {
	  
	  val shortNameProvinceNameList = for (provinceNode <- provincesNodeSeq;
	       shortName <- (provinceNode.attribute(SHORTNAME_ATTRIBUTE) :: Nil).flatten;
		   provinceName <- getShortAndUniqueNames(provinceNode) if 
		   						provinces.exists(_.id.equalsIgnoreCase(provinceName))
	  ) yield {
	    (shortName.toString, provinceName)
	  }
	  
	  Map(shortNameProvinceNameList: _*)
	}
	
	def getAdjacencies(adjacencyXML: Elem, provinces: Iterable[Province]): Iterable[Adjacency] = {
	  val provincesNodeSeq = adjacencyXML \\ PROVINCE_TAGNAME
	  val optionTTrait = new OptionTs {}
	  val provincesNodeSeqOptionT: OptionT[Seq, scala.xml.Node] = 
	    optionTTrait.optionT[Seq].apply(provincesNodeSeq.map(Some(_)))
	  
	  val shortNameToProvinceNameMap = getShortNameToProvinceNameMap(provincesNodeSeq, provinces)
	    
	  (for (provinceNode <- provincesNodeSeqOptionT;
		adjacencyNode <- getAdjacencyNodeSeqOptionT(provinceNode);
		shortName <- optionTTrait.optionT[Seq].apply(provinceNode.attribute(SHORTNAME_ATTRIBUTE).map(_.toString) :: Nil);
		provinceName <- optionTTrait.optionT[Seq].apply((shortNameToProvinceNameMap.get(shortName) :: Nil));
		coastType <- optionTTrait.optionT[Seq].apply(adjacencyNode.attribute(TYPE_ATTRIBUTE) :: Nil);
		neighbour <- getNeighbours(adjacencyNode);
		neighbourProvince <- optionTTrait.optionT[Seq].apply(shortNameToProvinceNameMap.get(neighbour) :: Nil)
	  ) yield (
	    coastType.toString match {
	      case LAND_TYPE => new Adjacency(provinceName, neighbourProvince, UnitType.ARMY)
	      case ALL_COAST_TYPE => new Adjacency(provinceName, neighbourProvince, UnitType.FLEET)
	      case _ => new Adjacency("%s-%s" format (provinceName, coastType.toString), 
	          neighbourProvince, UnitType.FLEET)
	    }
	  )).flatten
	}
}