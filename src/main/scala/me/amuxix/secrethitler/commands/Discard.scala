package me.amuxix.secrethitler.commands

import cats.effect.IO
import me.amuxix.secrethitler.{FascistPolicy, Game, GameWithChancellorElected, LiberalPolicy}
import me.amuxix.wrappers.MessageEvent

import scala.util.matching.Regex

object Discard extends SecretHitlerTextCommand {
  override def pattern: Regex = "^discard (fascist|liberal)$".r

  override protected def secretHitlerCommand(regex: Regex, event: MessageEvent, game: Game): IO[Option[Game]] = {
    val policy = event.content.toLowerCase match {
      case regex("fascist") => FascistPolicy
      case regex("liberal") => LiberalPolicy
    }
    game match {
      case game: GameWithChancellorElected => game.discardPolicy(policy)
      case _                               => IO.pure(Some(game))
    }
  }
}
