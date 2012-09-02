package com.squeryl.jdip.creators

import com.squeryl.jdip.tables.Adjacency
import scala.xml._
import scalaz._

object AdjacencyCreator {
	val PROVINCE_TAGNAME = "PROVINCE"
	val ADJACENCY_TAGNAME = "ADJACENCY"
	val SHORTNAME_ATTRIBUTE = "shortname"  
	val TYPE_ATTRIBUTE = "type"
	val REFS_ATTRIBUTE = "refs"
	  
	  
	def getAdjacencyNodeSeqOptionT(provinceNode: NodeSeq): OptionT[Seq, NodeSeq] = {
	  val optionTTrait = new OptionTs {}
	  val adjacenciesNodeSeq = provinceNode \\ ADJACENCY_TAGNAME
	  
	  optionTTrait.optionT[Seq].apply(adjacenciesNodeSeq.map(Some(_)))
	}
	
	def getNeighbours(adjacencyNode: NodeSeq): OptionT[Seq, NodeSeq] = {
	  val optionTTrait = new OptionTs {}
	  val refs = (adjacencyNode \\ ("@%s" format REFS_ATTRIBUTE)).toString().split(" ")
	  
	  
	}
	
	def getAdjacencies(adjacencyXML: Elem): Iterable[Adjacency] = {
	  val provincesNodeSeq = adjacencyXML \\ PROVINCE_TAGNAME
	  val optionTTrait = new OptionTs {}
	  val provincesNodeSeqOptionT = optionTTrait.optionT[Seq].apply(provincesNodeSeq.map(Some(_)))
	  
	  for (provinceNode <- provincesNodeSeqOptionT;
		adjacencyNode <- getAdjacencyNodeSeqOptionT(provinceNode);
		shortName <- optionTTrait.optionT[Seq].apply(provinceNode.attribute(SHORTNAME_ATTRIBUTE) :: Nil);
		coastType <- optionTTrait.optionT[Seq].apply(adjacencyNode.attribute(TYPE_ATTRIBUTE) :: Nil);
		
	  ) yield (
		new Adjacency()	  
	  )
	  
	  Nil
	}
}