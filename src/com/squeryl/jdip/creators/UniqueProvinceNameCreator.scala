package com.squeryl.jdip.creators

import com.squeryl.jdip.tables.UniqueProvinceName
import scalaz._

object UniqueProvinceNameCreator extends OptionTs {
  val PROVINCE_TAGNAME = "PROVINCE"
  val UNIQUENAME_TAGNAME = "UNIQUENAME"
  val NAME_ATTRIBUTE = "name"
  val SHORTNAME_ATTRIBUTE = "shortname"
    
  def getUniqueNamesAndShortName(pNode: scala.xml.Node): OptionT[Iterable, String] = {
    val uniqueNames = (pNode \\ UNIQUENAME_TAGNAME).map(_.attribute(NAME_ATTRIBUTE).map(_.toString)).toList
    val shortName = pNode.attribute(SHORTNAME_ATTRIBUTE).map(_.toString)
    
    optionT[Iterable].apply(shortName :: uniqueNames)
  }
    
  implicit def toOptionTFromOption[A](option: Option[A]) = optionT[Iterable].apply(option :: Nil)
  
  def getUniqueProvinceNames(adjacencies: scala.xml.Elem): Iterable[UniqueProvinceName] = {
    val provinceNodeSeq = adjacencies \\ PROVINCE_TAGNAME
    
    val provinceNodeOptionT = optionT[Iterable].apply(provinceNodeSeq.map(Some(_)))
    
    (for (pNode <- provinceNodeOptionT;
    	 uniqueName <- getUniqueNamesAndShortName(pNode);
    	 shortName <- pNode.attribute(SHORTNAME_ATTRIBUTE).map(_.toString)
    ) yield {new UniqueProvinceName(uniqueName, shortName)}).value.flatten
    
  }
  
}