package com.squeryl.jdip.creators

import scala.xml._
import com.squeryl.jdip.tables.Location

object LocationCreator {
  val PROVINCE_TAGNAME = "PROVINCE"
  val ADJACENCY_TAGNAME = "ADJACENCY"
  val SHORTNAME_ATTRIBUTE = "shortname"
  val TYPE_ATTRIBUTE = "type"
    
  def getLocationList(stdAdjacency: scala.xml.Elem): Iterable[Location] = {
    val provinceNodeSeq = stdAdjacency \\ PROVINCE_TAGNAME
    
    (for (provinceNode <- provinceNodeSeq;
    	 adjacencyNode <- (provinceNode \\ ADJACENCY_TAGNAME)
    ) yield {
      for(shortname <- provinceNode.attribute(SHORTNAME_ATTRIBUTE).map(_.toString);
    	  adjacencyType <- adjacencyNode.attribute(TYPE_ATTRIBUTE).map(_.toString)
      ) yield {
        new Location(shortname, adjacencyType)
      }
    }).flatten
  }
  
}