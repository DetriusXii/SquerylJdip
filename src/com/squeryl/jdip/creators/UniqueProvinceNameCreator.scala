package com.squeryl.jdip.creators

import com.squeryl.jdip.tables.UniqueProvinceName
import scalaz._
import scalaz.OptionT._

object UniqueProvinceNameCreator {
  val PROVINCE_TAGNAME = "PROVINCE"
  val UNIQUENAME_TAGNAME = "UNIQUENAME"
  val NAME_ATTRIBUTE = "name"
  val SHORTNAME_ATTRIBUTE = "shortname"
    
  def getUniqueNamesAndShortName(pNode: scala.xml.Node): OptionT[Iterable, String] = {
    val uniqueNames = (pNode \\ UNIQUENAME_TAGNAME).map(_.attribute(NAME_ATTRIBUTE).map(_.toString)).toList
    val shortName = pNode.attribute(SHORTNAME_ATTRIBUTE).map(_.toString)
    
    OptionT(shortName :: uniqueNames)
  }
    
  implicit def toOptionTFromOption[A](option: Option[A]) = OptionT(option :: Nil)
  
  implicit val seqInstance = new Monad[Seq] {
    def point[A](a : => A): Seq[A] = a :: Nil
    
    def bind[A, B](fa: Seq[A])(f : A => Seq[B]): Seq[B] = fa.flatMap(f)
  }
  
  implicit val monadInstance = new Monad[Iterable] {
    def point[A](a : => A): Iterable[A] = a :: Nil
    def bind[A, B](fa: Iterable[A])(f : A => Iterable[B]): Iterable[B] = fa.flatMap(f)
  }
  
  def getUniqueProvinceNames(adjacencies: scala.xml.Elem): Iterable[UniqueProvinceName] = {
    val provinceNodeSeq = adjacencies \\ PROVINCE_TAGNAME
    
    val provinceNodeOptionT = 
      OptionT[Iterable, scala.xml.Node](provinceNodeSeq.map(Some(_)))
    
    (for (pNode <- provinceNodeOptionT;
    	 uniqueName <- getUniqueNamesAndShortName(pNode);
    	 shortName <- OptionT[Iterable, String](
    	     pNode.attribute(SHORTNAME_ATTRIBUTE).map(_.toString) :: Nil)
    ) yield {new UniqueProvinceName(uniqueName, shortName)}).run.flatten
    
  }
  
}