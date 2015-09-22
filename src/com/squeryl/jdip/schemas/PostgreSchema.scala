/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.squeryl.jdip.schemas

import com.squeryl.jdip.tables.Schemata
import org.squeryl._
import com.squeryl.jdip.adapters.RevisedPostgreSqlAdapter
import java.sql.SQLException
import org.squeryl.PrimitiveTypeMode._

class PostgreSchema(val schemaName: String) extends Schema {
  override def name = Some(schemaName)
  
  override def create = {
    val schemaCount = InformationSchema.schemata.where(s => s.schema_name === schemaName).size
    
    if (schemaCount == 0 ) {
      val session = Session.currentSession
      val dbAdapter = session.databaseAdapter
      
      dbAdapter match {
        case psqlAdapter: RevisedPostgreSqlAdapter =>
          try {
            psqlAdapter.executeCreateSchema(session, schemaName)
            
          } catch {
            case sqlEx: SQLException => println("Could not create the schema %s" format schemaName)
              sqlEx.printStackTrace
          }
      }
    }
    
    super.create
  }
  
  override def drop = {
    val schemaCount = InformationSchema.schemata.where(s => s.schema_name === schemaName).size
    
    if ( schemaCount > 0) {
      super.drop
      
      val session = Session.currentSession
      val dbAdapter = session.databaseAdapter
      
      dbAdapter match {
        case psqlAdapter: RevisedPostgreSqlAdapter =>
          try {
            psqlAdapter.executeDropSchema(session, schemaName)
          } catch {
            case sqlEx: SQLException => println("Could not create the schema %s" format schemaName)
          }
      }
    }
  }
}
