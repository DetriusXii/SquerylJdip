package com.squeryl.jdip.creators

object PlayerCreator {
	private val players: List[String] = (0 until 25) map ((u: Int) =>  "player%d" format u)
	private val passwords: List[String] = (0 until 25) map ((u: Int) => "password%d")
	
}