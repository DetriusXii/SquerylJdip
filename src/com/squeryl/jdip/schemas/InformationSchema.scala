/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.squeryl.jdip.schemas

import org.squeryl._
import com.squeryl.jdip.tables._

object InformationSchema extends Schema {
  val schemata = table[Schemata]("schemata", "information_schema")
}
