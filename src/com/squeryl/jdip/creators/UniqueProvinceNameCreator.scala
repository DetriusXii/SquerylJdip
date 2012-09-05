package com.squeryl.jdip.creators

import com.squeryl.jdip.tables.UniqueProvinceName
import scalaz._

object UniqueProvinceNameCreator {
  val PROVINCE_TAGNAME = "PROVINCE"
  val UNIQUENAME_TAGNAME = "UNIQUENAME"
  val NAME_ATTRIBUTE = "name"
  val SHORTNAME_ATTRIBUTE = "shortname"
    
  def getUniqueProvinceNames(adjacencies: scala.xml.Elem): Iterable[UniqueProvinceName] = {
    val provinceNodeSeq = adjacencies \\ PROVINCE_TAGNAME
    val optionTTrait = new OptionTs {}
    
    val provinceNodeOptionT = optionTTrait.optionT[Iterable].apply(provinceNodeSeq.map(Some(_)))
    
    (for (pNode <- provinceNodeOptionT;
    	 uniquenameNode <- optionTTrait.optionT[Iterable].apply((pNode \\ UNIQUENAME_TAGNAME).map(Some(_)));
    	 shortName <- optionTTrait.optionT[Iterable].apply(pNode.attribute(SHORTNAME_ATTRIBUTE) :: Nil);
    	 uniqueName <- optionTTrait.optionT[Iterable].
    	 	apply(Some(shortName) :: uniquenameNode.attribute(NAME_ATTRIBUTE) :: Nil)
    ) yield (new UniqueProvinceName(uniqueName.toString, shortName.toString))).value.flatten
    
  }
  
}