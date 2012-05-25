/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.squeryl.jdip.adapters

import org.squeryl.Table
import org.squeryl.internals.FieldMetaData
import java.sql._
import org.squeryl.Session

class RevisedPostgreSqlAdapter extends org.squeryl.adapters.PostgreSqlAdapter {
  override def writeUniquenessConstraint(t: Table[_], cols: Iterable[FieldMetaData]) = {
              //ALTER TABLE TEST ADD CONSTRAINT NAME_UNIQUE UNIQUE(NAME)
              val sb = new StringBuilder(256)

              sb.append("alter table ")
              sb.append(quoteName(t.prefixedName))
              sb.append(" add constraint ")
              sb.append(quoteName(t.name + "CPK"))
              sb.append(" unique(")
              sb.append(cols.map(_.columnName).map(quoteName(_)).mkString(","))
              sb.append(")")
              sb.toString
            } 
            
  @throws(classOf[SQLException])
  def executeCreateSchema(session: Session, schemaName: String) = {
    val st = session.connection.prepareStatement("CREATE SCHEMA %s" format schemaName)
    
    try {
      st.execute
    } finally {
      st.close
    }
  }
  
  @throws(classOf[SQLException])
  def executeDropSchema(session: Session, schemaName: String) = {
    val st = session.connection.prepareStatement("DROP SCHEMA %s CASCADE" format schemaName)
    try {
      st.execute
    } finally {
      st.close
    }
    
  }
}
