package me.amuxix.commands

import cats.effect.IO
import me.amuxix.{AnyCommand, Bot, Persistence}
import me.amuxix.syntax.all.*
import me.amuxix.wrappers.event.MessageEvent

import scala.util.Try
import scala.util.matching.Regex

object CopyAllowedCommands extends TextCommand:
  override def pattern: Regex = "^copy allowed commands (.+)$".r

  override protected def apply(regex: Regex, event: MessageEvent): IO[Boolean] =
    event.content match
      case regex(id) =>
        Try(id.toLong).toOption
          .fold(event.jda.getChannelByName(id))(event.jda.getChannelByID)
          .flatMap {
            case None => event.reply(s"Could not find channel with provided name or ID.")
            case Some(channel) =>
              for
                _ <- Bot.enabledCommands.update { enabledCommands =>
                  val option = enabledCommands.get(channel.id)
                  enabledCommands + option.fold(event.channel.id -> Set.empty[AnyCommand])(event.channel.id -> _)
                }
                _ <- event.reply(s"Copy successful.")
                _ <- Persistence.saveEnabledCommands
              yield ()
          }
          .as(true)
      case _ => IO.pure(false)

  override val description: String = "Copies the allowed commands from the given channel id or name."
