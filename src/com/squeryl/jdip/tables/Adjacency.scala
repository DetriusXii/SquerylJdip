package com.squeryl.jdip.tables

import org.squeryl.KeyedEntity

class Adjacency(val srcProvince: String, val dstProvince: String, val unitType: String) extends KeyedEntity[Int] {
	val id = 0
	
	def this() = this("", "", "")
}