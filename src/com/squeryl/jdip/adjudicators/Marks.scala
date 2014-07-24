package com.squeryl.jdip.adjudicators

sealed abstract class Mark
case class NoMark extends Mark
case class ConvoyEndangered extends Mark
case class Cut extends Mark
case class Void extends Mark
case class ConvoyUnderAttack extends Mark