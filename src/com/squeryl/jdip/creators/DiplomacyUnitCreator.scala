package com.squeryl.jdip.creators
import scala.xml._
import com.squeryl.jdip.tables.DiplomacyUnit
import scalaz._
import com.squeryl.jdip.tables.GamePlayerEmpire
import com.squeryl.jdip.tables.GameTime
import java.io.File

object DiplomacyUnitCreator {
	val VARIANTS_FILE_NAME = "/home/detriusxiiuser/jdip/variants/stdVariants/variants.xml"
	val VARIANT_TAGNAME = "VARIANT"
	val APPSETTING_TAGNAME =  "appsetting"
	val NAME_TAGNAME = "name"
	val VALUE_TAGNAME = "value"
	val VARIANTS_XML_SEARCHPATHS = "VARIANTS_XML_SEARCHPATHS"
	val VARIANT_FILENAME = "VARIANT_FILENAME"
	val STANDARD_VARIANT_NAME = "Standard"
	val INITIALSTATE_TAGNAME = "INITIALSTATE"
	val PROVINCE_ATTRIBUTE = "province"
	val POWER_ATTRIBUTE = "power"
	val UNIT_ATTRIBUTE = "unit"
	val UNITCOAST_ATTRIBUTE = "unitcoast"
	
	def findFirstVariant(configFilename: String) : Elem = {
	  val configuration = XML.load(configFilename)
	  val appsettingNodeseq = configuration \\ APPSETTING_TAGNAME
	  
	  val searchPathsListT = appsettingNodeseq.find(_.child.exists(u => {
	    u.label.equals(NAME_TAGNAME) && u.text.equals(VARIANTS_XML_SEARCHPATHS)
	  })).map(_ \\ VALUE_TAGNAME map (u => u.text)) match {
	    case Some(u: Seq[String]) => 
	      u.foldRight(ListT.empty[Option, String])(
	          (u: String, v: ListT[Option, String]) => u :: v)
	    case None => ListT[Option, String](None)
	  }
	  
	  val variantFilenamesListT = appsettingNodeseq.find(_.child.exists(u => {
	    u.label.equals(NAME_TAGNAME) && u.text.equals(VARIANT_FILENAME)
	  })).map(_ \\ VALUE_TAGNAME map (u => u.text)) match {
	    case Some(u: Seq[String]) =>
	      u.foldRight(ListT.empty[Option, String])(
	    		  (u: String, v: ListT[Option, String]) => u :: v
	      )
	    case None => ListT[Option, String](None)
	  }
	  
	  for ( searchPath <- searchPathsListT;
			variantFilename <- variantFilenamesListT;
			searchFile <- new File(searchPath) :: ListT.empty[Option, File] if 
				searchFile.exists && searchFile.isDirectory
				
	  ) yield (
			searchPath
	  )
	  
	  
	  for (searchPaths <- searchPathsOption;
		   variantFilename <- variantFilenameOption;
		   	_ <- searchPaths.find((directoryPath: String) =>{
		   	  val searchDirectory = new java.io.File(directoryPath)
		   	  searchDirectory.exists() && searchDirectory.isDirectory()
		   	})
	  ) yield {
	    
	  }
	  val fileFoundOption = searchPathsOption.flatMap((u: Seq[String]) => {
	    u.find((v: String) => {
	      val searchDirectory = new java.io.File(v)
	      searchDirectory.exists() && searchDirectory.isDirectory() && 
	      	searchDirectory.listFiles().find((f: java.io.File) => {
	      	  f.getName().equals("")
	      	})
	      
	    })
	  })
	  
	  <a></a>
	}  
	  
	def getDiplomacyUnits(gamePlayerEmpires: Iterable[GamePlayerEmpire], gameTime: GameTime): Iterable[DiplomacyUnit] = {
	  val variantsXML = XML.load(VARIANTS_FILE_NAME)
	  
	  val variantNode = (variantsXML \\ VARIANT_TAGNAME).filter(_ match {
	    case u: Elem => u.attribute(NAME_ATTRIBUTE).toString.equals(STANDARD_VARIANT_NAME)
	    case _ => false
	  }).head
	  
	  val variantElem = variantNode match {
	    case u: Elem => Some(u)
	    case _ => None
	  }
	  
	  
	  variantElem.flatMap((u: Elem) => {
	    val initialStateProjection = u \\ INITIALSTATE_TAGNAME
	    val unitNumberWithInitialStateProjection = 
	    	(0 until initialStateProjection.length) zip initialStateProjection
	    val diplomacyUnitListOption = unitNumberWithInitialStateProjection map (_ match {
	      case (unitNumber: Int, initialStateElem: Elem) => 
	        for (province <- u.attribute(PROVINCE_ATTRIBUTE);
				  power <- u.attribute(POWER_ATTRIBUTE);
				  unitType <- u.attribute(UNIT_ATTRIBUTE);
				  unitCoast <- u.attribute(UNITCOAST_ATTRIBUTE);
				  owner <- gamePlayerEmpires.find(gpe => gpe.empireName.equals(power.toString))
			) yield {
			  val location = u.attribute(UNITCOAST_ATTRIBUTE) match {
			    case Some(coast) => "%s-%s" format (province.toString, coast.toString)
			    case None => province.toString
			  }
			  new DiplomacyUnit(unitType.toString, owner.id, location, unitNumber, gameTime.id)
			}
	      case _ => None
	    })
	    Some(diplomacyUnitListOption.flatten)
	  }) match {
	    case Some (u) => u
	    case None => Nil
	  }
	}
}