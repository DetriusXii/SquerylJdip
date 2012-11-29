package com.squeryl.jdip
import scala.xml._
import scalaz._
import java.io.File
import javax.xml.parsers._


object ConfigXMLLoader {
	val APPSETTING_TAGNAME =  "appsetting"
	val NAME_TAGNAME = "name"
	val VALUE_TAGNAME = "value"
	val VARIANTS_XML_SEARCHPATHS = "VARIANTS_XML_SEARCHPATHS"
	val VARIANT_FILENAME = "VARIANT_FILENAME"
	val JDIP_MAP_SVG_FILENAME = "JDIP_MAP_SVG_FILENAME"
	val ADJACENCY_FILENAME = "ADJACENCY_FILENAME"
	val COMBINED_SVG_FULLFILENAME = "COMBINED_SVG_FULLFILENAME"
	  
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
	  
	  val saxParser = SAXParserFactory.newInstance.newSAXParser
	  absolutePathsListT.head.map(XML.withSAXParser(saxParser).load(_))
	}
	
	def findFirstAdjacency(configFilepath: String): Option[Elem] = {
	  val configuration = XML.load(configFilepath)
	  val appsettingNodeSeq = configuration \\ APPSETTING_TAGNAME
	  
	  val searchPathsListT = appsettingNodeSeq.find(_.child.exists(u =>
	  	u.label.equals(NAME_TAGNAME) && u.text.equals(VARIANTS_XML_SEARCHPATHS)
	  )).map(_ \\ VALUE_TAGNAME map (_.text)) match {
	    case Some(u: Seq[_]) => u.foldRight(ListT.empty[Option, String])((u, v) => u :: v)
	    case None => ListT[Option, String](None)
	  }
	  
	  val adjacencyFilenamesListT = appsettingNodeSeq.find(_.child.exists(u =>
	  	u.label.equals(NAME_TAGNAME) && u.text.equals(ADJACENCY_FILENAME)
	  )).map(_ \\ VALUE_TAGNAME map (_.text)) match {
	    case Some(u: Seq[_]) => u.foldRight(ListT.empty[Option, String])((u, v) => u :: v)
	    case None => ListT[Option, String](None)
	  }
	  
	  val absolutePathsListT = for (searchPath <- searchPathsListT;
			  adjacencyFilename <- adjacencyFilenamesListT;
			  searchFile <- new File(searchPath) :: ListT.empty[Option, File] if
			  	searchFile.exists && searchFile.isDirectory && searchFile.list.exists((filename: String) =>
			  		filename.equals(adjacencyFilename)
			  	)) yield ((new File(searchFile, adjacencyFilename)).getAbsolutePath)
			  	
	  absolutePathsListT.head.map(XML.load(_))
	}

	
	def findFirstCombinedSVG(configFilepath: String): Option[String] = {
	  val configuration = XML.load(configFilepath)
	  val appSettingNodeSeq = configuration \\ APPSETTING_TAGNAME
	  
	  val combinedSVGAppsettingNode = appSettingNodeSeq.find(_.child.exists(u =>
	  	u.label.equals(NAME_TAGNAME) && u.text.equals(COMBINED_SVG_FULLFILENAME)
	  ))
	  
	  val filePaths = 
	    combinedSVGAppsettingNode.map(_ \\ VALUE_TAGNAME map (_.text)).flatten
	  filePaths.find(filepath => {
	    val file = new File(filepath.trim())
	    val exists = file.exists
	    val isFile = file.isFile
	    file.exists && file.isFile
	  })
	}
}