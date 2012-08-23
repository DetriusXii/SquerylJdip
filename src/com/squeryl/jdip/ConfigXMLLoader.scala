package com.squeryl.jdip
import scala.xml._
import scalaz._
import java.io.File



object ConfigXMLLoader {
	val APPSETTING_TAGNAME =  "appsetting"
	val NAME_TAGNAME = "name"
	val VALUE_TAGNAME = "value"
	val VARIANTS_XML_SEARCHPATHS = "VARIANTS_XML_SEARCHPATHS"
	val VARIANT_FILENAME = "VARIANT_FILENAME"
	val JDIP_MAP_SVG_FILENAME = "JDIP_MAP_SVG_FILENAME"
	  
	 def findFirstVariant(configFilepath: String) : Elem = {
	  val configuration = XML.load(configFilepath)
	  val appsettingNodeseq = configuration \\ APPSETTING_TAGNAME
	  
	  val searchPathsListT = appsettingNodeseq.find(_.child.exists(u => {
	    u.label.equals(NAME_TAGNAME) && u.text.equals(VARIANTS_XML_SEARCHPATHS)
	  })).map(_ \\ VALUE_TAGNAME map (u => u.text)) match {
	    case Some(u: Seq[_]) => 
	      u.foldRight(ListT.empty[Option, String])(
	          (u: String, v: ListT[Option, String]) => u :: v)
	    case None => ListT[Option, String](None)
	  }
	  
	  val variantFilenamesListT = appsettingNodeseq.find(_.child.exists(u => {
	    u.label.equals(NAME_TAGNAME) && u.text.equals(VARIANT_FILENAME)
	  })).map(_ \\ VALUE_TAGNAME map (u => u.text)) match {
	    case Some(u: Seq[_]) =>
	      u.foldRight(ListT.empty[Option, String])(
	    		  (u: String, v: ListT[Option, String]) => u :: v
	      )
	    case None => ListT[Option, String](None)
	  }
	  
	  val absolutePathsListT = for ( searchPath <- searchPathsListT;
			variantFilename <- variantFilenamesListT;
			searchFile <- new File(searchPath) :: ListT.empty[Option, File] if 
				searchFile.exists && searchFile.isDirectory && searchFile.list.exists((filename: String) => {
				  filename.equals(variantFilename)
				})
	  ) yield (
			new File(searchFile, variantFilename).getAbsolutePath
	  )
	  
	  absolutePathsListT.head match {
	    case Some(u: String) => XML.load(u)
	    case None => <a></a>
	  }
	}
	
	def findFirstSVG(configFilepath: String): Option[Elem] = {
	  val configuration = XML.load(configFilepath)
	  val appsettingNodeSeq = configuration \\ APPSETTING_TAGNAME
	  
	  val searchPathsListT = appsettingNodeSeq.find(_.child.exists(u => 
	    u.label.equals(NAME_TAGNAME) && u.text.equals(VARIANTS_XML_SEARCHPATHS)
	  )).map(_ \\ VALUE_TAGNAME map (_.text)) match {
	    case Some(u: Seq[_]) => u.foldRight(ListT.empty[Option, String])((u, v) => u :: v)
	    case None => ListT[Option, String](None)
	  }
	  
	  val svgFilenamesListT = appsettingNodeSeq.find(_.child.exists(u => 
	  	u.label.equals(NAME_TAGNAME) && u.text.equals(JDIP_MAP_SVG_FILENAME)
	  )).map(_ \\ VALUE_TAGNAME map (_.text)) match {
	    case Some(u: Seq[_]) => u.foldRight(ListT.empty[Option, String])((u, v) => u :: v)
	    case None => ListT[Option, String](None)
	  }
	  
	  val absolutePathsListT = for ( searchPath <- searchPathsListT;
			  svgFilename <- svgFilenamesListT;
			  searchFile <- new File(searchPath) :: ListT.empty[Option, File] if 
			  	searchFile.exists && searchFile.isDirectory && searchFile.list.exists((filename: String) => 
			  		filename.equals(svgFilename)
			  	) ) yield ((new File(searchFile, svgFilename)).getAbsolutePath
			  	)
	  
	  absolutePathsListT.head.map(XML.load(_))
	}
}