package me.amuxix.secrethitler

sealed abstract class Policy(override val toString: String, val emote: String)
object LiberalPolicy extends Policy("Liberal", "\uD83C\uDDF1")
object FascistPolicy extends Policy("Fascist", "\uD83C\uDDEB")
