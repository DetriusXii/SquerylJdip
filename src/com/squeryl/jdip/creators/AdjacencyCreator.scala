package com.squeryl.jdip.creators

import com.squeryl.jdip.tables.Adjacency
import scala.xml._
import scalaz._
import com.squeryl.jdip.tables.UnitType

object AdjacencyCreator {
	val PROVINCE_TAGNAME = "PROVINCE"
	val ADJACENCY_TAGNAME = "ADJACENCY"
	val SHORTNAME_ATTRIBUTE = "shortname"  
	val TYPE_ATTRIBUTE = "type"
	val REFS_ATTRIBUTE = "refs"
	val LAND_TYPE = "mv"
	val ALL_COAST_TYPE = "xc"
	  
	def getAdjacencyNodeSeqOptionT(provinceNode: scala.xml.Node): OptionT[Seq, scala.xml.Node] = {
	  val optionTTrait = new OptionTs {}
	  val adjacenciesNodeSeq = provinceNode \\ ADJACENCY_TAGNAME
	  
	  optionTTrait.optionT[Seq].apply(adjacenciesNodeSeq.map(Some(_)))
	}
	
	def getNeighbours(adjacencyNode: scala.xml.Node): OptionT[Seq, String] = {
	  val optionTTrait = new OptionTs {}
	  val refs = adjacencyNode.attribute(REFS_ATTRIBUTE).toString().split(" ").toSeq
	  optionTTrait.optionT[Seq].apply(refs.map(Some(_)))
	}
	
	def getAdjacencies(adjacencyXML: Elem): Iterable[Adjacency] = {
	  val provincesNodeSeq = adjacencyXML \\ PROVINCE_TAGNAME
	  val optionTTrait = new OptionTs {}
	  val provincesNodeSeqOptionT: OptionT[Seq, scala.xml.Node] = 
	    optionTTrait.optionT[Seq].apply(provincesNodeSeq.map(Some(_)))
	  
	  (for (provinceNode <- provincesNodeSeqOptionT;
		adjacencyNode <- getAdjacencyNodeSeqOptionT(provinceNode);
		shortName <- optionTTrait.optionT[Seq].apply(provinceNode.attribute(SHORTNAME_ATTRIBUTE) :: Nil);
		coastType <- optionTTrait.optionT[Seq].apply(adjacencyNode.attribute(TYPE_ATTRIBUTE) :: Nil);
		neighbour <- getNeighbours(adjacencyNode)
	  ) yield (
	    coastType.toString match {
	      case LAND_TYPE => new Adjacency(shortName.toString, neighbour.toString, UnitType.ARMY)
	      case ALL_COAST_TYPE => new Adjacency(shortName.toString, neighbour.toString, UnitType.FLEET)
	      case _ => new Adjacency("%s-%s" format (shortName.toString, coastType.toString), neighbour.toString, UnitType.FLEET)
	    }
	  )).flatten
	}
}