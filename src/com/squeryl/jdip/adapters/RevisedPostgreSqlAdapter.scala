/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.squeryl.jdip.adapters

import org.squeryl.Table
import org.squeryl.internals.FieldMetaData
import java.sql._
import org.squeryl.Session
import org.squeryl.annotations._
import org.squeryl.internals.StatementWriter

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
  
  override def postCreateTable(t: Table[_], printSinkWhenWriteOnlyMode: Option[String => Unit]) = {

    val autoIncrementedFields = t.posoMetaData.fieldsMetaData.filter(_.isAutoIncremented)

    
    for(fmd <-autoIncrementedFields) {
      val sw = new StatementWriter(false, this)
      val sequenceName = t.prefix match {
        case Some(prefix: String) => "%s.%s" format (t.schema, fmd.sequenceName)
        case None => fmd.sequenceName
      }
      
      sw.write("create sequence ", 
          quoteName(sequenceName))

      if(printSinkWhenWriteOnlyMode == None) {
        val st = Session.currentSession.connection.createStatement
        st.execute(sw.statement)
      }
      else
        printSinkWhenWriteOnlyMode.get.apply(sw.statement + ";")
    }
  }
  
   override def postDropTable(t: Table[_]) = {
    
    val autoIncrementedFields = t.posoMetaData.fieldsMetaData.filter(_.isAutoIncremented)

    for(fmd <-autoIncrementedFields) {
      val sequenceName = t.prefix match {
        case Some(prefix: String) => "%s.%s" format (prefix, fmd.sequenceName) 
        case None => fmd.sequenceName
      }
      
      execFailSafeExecute("drop sequence " + quoteName(sequenceName), e=>e.getSQLState.equals("42P01"))
    }
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
