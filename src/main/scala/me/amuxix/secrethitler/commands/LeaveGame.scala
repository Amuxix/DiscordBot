package me.amuxix.secrethitler.commands

import cats.effect.IO
import me.amuxix.secrethitler.{CreatedGame, Game}
import me.amuxix.wrappers.MessageEvent

import scala.util.matching.Regex

object LeaveGame extends SecretHitlerTextCommand {
  override def pattern: Regex = "^leave game$".r

  override protected def secretHitlerCommand(regex: Regex, event: MessageEvent, game: Game): IO[Option[Game]] =
    for {
      _ <- event.sendMessage(s"${event.authorName} left the game.")
      players = game.players - event.author
    } yield Some(new CreatedGame(players, game.channel, event.guild.get))
}
