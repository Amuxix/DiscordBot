package me.amuxix.secrethitler

import me.amuxix.Named

sealed abstract class Role(val role: String) extends Named
case object Liberal extends Role("Liberal")
case object Fascist extends Role("Fascist")
case object Hitler extends Role("Fascist")
