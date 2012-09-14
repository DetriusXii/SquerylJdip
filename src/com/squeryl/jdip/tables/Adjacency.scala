package com.squeryl.jdip.tables

import org.squeryl.KeyedEntity
import org.squeryl.dsl._
import org.squeryl.PrimitiveTypeMode._

case class Adjacency(srcLocation: Int, 
    dstLocation: Int) extends KeyedEntity[CompositeKey2[Int, Int]] {
	def id = compositeKey(srcLocation, dstLocation)
	
	def this() = this(0, 0)
}