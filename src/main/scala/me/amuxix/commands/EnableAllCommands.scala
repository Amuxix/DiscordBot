package me.amuxix.commands

import cats.effect.IO
import me.amuxix.wrappers.event.MessageEvent
import me.amuxix.{Bot, Persistence}

import scala.util.matching.Regex

object EnableAllCommands extends TextCommand:
  override def pattern: Regex = "^enable all commands$".r

  override protected def apply(regex: Regex, event: MessageEvent): IO[Boolean] =
    for
      _ <- Bot.enabledCommands.update { enabledCommandsMap =>
        enabledCommandsMap + (event.channel.id -> Bot.allCommands.toList.toSet)
      }
      _ <- event.reply(s"Enabled all commands.")
      _ <- Persistence.saveEnabledCommands
    yield true

  override val description: String = "Disables all commands that can be disabled"
