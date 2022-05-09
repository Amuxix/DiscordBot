package me.amuxix.secrethitler.commands

import cats.effect.IO
import me.amuxix.Bot.userMap
import me.amuxix.commands.Command
import me.amuxix.secrethitler.{Game, GameBeforeNomination}
import me.amuxix.syntax.all.*
import me.amuxix.wrappers.MessageEvent

import scala.util.matching.Regex

object NominateChancellor extends SecretHitlerTextCommand {
  override def pattern: Regex = s"^nominate ${Command.userID}$$".r

  override protected def secretHitlerCommand(regex: Regex, event: MessageEvent, game: Game): IO[Option[Game]] =
    event.content.toLowerCase match {
      case regex(id) =>
        event.jda.getUserByID(id.toLong).flatMap { user =>
          game match {
            case game: GameBeforeNomination => game.nominateChancellor(user)
            case _                          => IO.pure(Some(game))
          }
        }
    }
}
