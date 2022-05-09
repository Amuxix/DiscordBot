package me.amuxix.secrethitler.commands

import cats.effect.IO
import me.amuxix.secrethitler.{FascistPolicy, LiberalPolicy}
import me.amuxix.secrethitler.{Game, GameWaitingForChancellorDecision}
import me.amuxix.wrappers.MessageEvent

import scala.util.matching.Regex

object Enact extends SecretHitlerTextCommand:
  override def pattern: Regex = "^enact (fascist|liberal)$".r

  override protected def secretHitlerCommand(regex: Regex, event: MessageEvent, game: Game): IO[Option[Game]] =
    val policy = event.content.toLowerCase match
      case regex("fascist") => FascistPolicy
      case regex("liberal") => LiberalPolicy
    game match
      case game: GameWaitingForChancellorDecision => game.enactPolicy(policy)
      case _                                      => IO.pure(Some(game))
