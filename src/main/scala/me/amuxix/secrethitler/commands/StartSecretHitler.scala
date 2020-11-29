package me.amuxix.secrethitler.commands

import cats.effect.IO
import me.amuxix.secrethitler.{CreatedGame, Game}
import me.amuxix.wrappers.MessageEvent

import scala.util.matching.Regex

object StartSecretHitler extends SecretHitlerTextCommand {
  override def pattern: Regex = "^start game$".r

  override protected def secretHitlerCommand(regex: Regex, event: MessageEvent, game: Game): IO[Option[Game]] =
    game match {
      case game: CreatedGame => game.start
      case _                 => IO.pure(Some(game))
    }
}
