package com.squeryl.jdip.creators

import com.squeryl.jdip.tables.Adjacency
import com.squeryl.jdip.tables.Location
import scala.xml._
import scalaz._
import com.squeryl.jdip.tables.UnitType

object AdjacencyCreator extends OptionTs {
	val PROVINCE_TAGNAME = "PROVINCE"
	val ADJACENCY_TAGNAME = "ADJACENCY"
	val UNIQUENAME_TAGNAME = "UNIQUENAME"
	val SHORTNAME_ATTRIBUTE = "shortname"  
	val TYPE_ATTRIBUTE = "type"
	val REFS_ATTRIBUTE = "refs"
	val NAME_ATTRIBUTE = "name"
	val LAND_TYPE = "mv"
	val ALL_COAST_TYPE = "xc"
	
	private def getAdjacencyNode(provinceNode: scala.xml.Node): OptionT[Iterable, scala.xml.Node] = {
	  val adjacencyNodeSeq: scala.xml.NodeSeq =
	    provinceNode \\ ADJACENCY_TAGNAME
	  
	  optionT[Iterable](adjacencyNodeSeq.map(Some(_)).toList)
	}
	
	private def getNeighbours(adjacencyNode: scala.xml.Node): OptionT[Iterable, String] = {
	  val refs = adjacencyNode.attribute(REFS_ATTRIBUTE).map(_.toString) match {
	    case Some(u: String) => u.split(" ").toList
	    case None => Nil
	  }
	  
	  optionT[Iterable](refs.map(Some(_)))
	}
	
	private def getNeighbourLocation(coastType: String,
									neighbourName: String,
									locations: Iterable[Location]): Option[Location] = {
	  if (neighbourName.contains("-")) {
	    val split = neighbourName.split("-")
	    val provinceName = split(0)
	    val coast = split(1)
	    
	    locations.find(_ match {
	      case Location(provinceName, coast) => true
	      case _ => false
	    })
	  } else {
	    locations.find(_ match {
	      case Location(neighbourName, coastType) => true
	      case _ => false
	    })
	  }
	}
	
	implicit def toOptionTFromOption[Q](option: Option[Q]): OptionT[Iterable, Q] =
	  optionT[Iterable](option :: Nil)
	
	def getAdjacencies(adjacencyXML: Elem, locations: Iterable[Location]): Iterable[Adjacency] = {
	  val provincesNodeSeq = adjacencyXML \\ PROVINCE_TAGNAME
	  val seqOptionT = optionT[Seq]
	  val provincesNodeSeqOptionT: OptionT[Iterable, scala.xml.Node] = 
	    optionT[Iterable](provincesNodeSeq.map(Some(_)))
	  
	    
	  (for (provinceNode <- provincesNodeSeqOptionT;
		shortname <- toOptionTFromOption(provinceNode.attribute(SHORTNAME_ATTRIBUTE).map(_.toString));
		adjacencyNode <- getAdjacencyNode(provinceNode);
		neighbour <- getNeighbours(adjacencyNode);
		coastType <- adjacencyNode.attribute(TYPE_ATTRIBUTE).map(_.toString);
		neighbourLocation <- getNeighbourLocation(coastType, neighbour, locations);
		srcLocation <- locations.find(_ match {
		  case Location(shortname, coastType) => true
		  case _ => false
		})
	  ) yield (
	    new Adjacency(srcLocation.id, neighbourLocation.id)
	  )).value.flatten
	}
}