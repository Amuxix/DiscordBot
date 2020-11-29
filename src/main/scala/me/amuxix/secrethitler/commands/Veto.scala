package me.amuxix.secrethitler.commands

import cats.effect.IO
import me.amuxix.secrethitler.{Game, GameWaitingForChancellorDecision}
import me.amuxix.wrappers.MessageEvent

import scala.util.matching.Regex

object Veto extends SecretHitlerTextCommand {
  override def pattern: Regex = "^I wish to veto this agenda$".r

  override protected def secretHitlerCommand(regex: Regex, event: MessageEvent, game: Game): IO[Option[Game]] =
    game match {
      case game: GameWaitingForChancellorDecision => game.veto
      case _                                      => IO.pure(Some(game))
    }
}
