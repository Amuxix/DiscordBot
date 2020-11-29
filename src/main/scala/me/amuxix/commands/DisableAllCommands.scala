package me.amuxix.commands

import cats.effect.IO
import me.amuxix.{Bot, Persistence}
import me.amuxix.wrappers.MessageEvent

import scala.util.matching.Regex

object DisableAllCommands extends TextCommand {
  override def pattern: Regex = "^disable all commands$".r

  override protected def apply(regex: Regex, event: MessageEvent): IO[Boolean] =
    for {
      _ <- Bot.enabledCommands.update { enabledCommandsMap =>
        enabledCommandsMap + (event.channel.id -> Set.empty)
      }
      _ <- event.sendMessage(s"Disabled all commands.")
      _ <- Persistence.saveEnabledCommands
    } yield true

  override val description: String = "Enables all commands"
}
