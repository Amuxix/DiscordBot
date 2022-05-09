package me.amuxix.secrethitler.commands.policies

import cats.effect.IO
import me.amuxix.Bot.userMap
import me.amuxix.commands.Command
import me.amuxix.secrethitler.commands.SecretHitlerTextCommand
import me.amuxix.secrethitler.{Game, WaitingForPolicyResolution}
import me.amuxix.syntax.all.*
import me.amuxix.wrappers.MessageEvent

import scala.util.matching.Regex

object Kill extends SecretHitlerTextCommand:
  override def pattern: Regex = s"^kill ${Command.userID}$$".r

  override protected def secretHitlerCommand(regex: Regex, event: MessageEvent, game: Game): IO[Option[Game]] =
    event.content.toLowerCase match
      case regex(id) =>
        event.jda.getUserByID(id.toLong).flatMap { target =>
          game match
            case game: WaitingForPolicyResolution => game.kill(target)
            case _                                => IO.pure(Some(game))
        }
