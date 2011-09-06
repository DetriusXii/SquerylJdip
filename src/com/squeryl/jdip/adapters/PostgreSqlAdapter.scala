/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.squeryl.jdip.adapters

import org.squeryl.Table
import org.squeryl.internals.FieldMetaData

class PostgreSqlAdapter extends org.squeryl.adapters.PostgreSqlAdapter {
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
}
