package me.amuxix.commands

import cats.effect.IO
import me.amuxix.{Bot, Persistence}
import me.amuxix.wrappers.MessageEvent

import scala.util.matching.Regex

object EnableAllCommands extends Command {
  override def regex: Regex = "^enable all commands$".r

  override protected def apply(regex: Regex, event: MessageEvent): IO[Boolean] =
    for {
      _ <- Bot.enabledCommands.update { enabledCommandsMap =>
        enabledCommandsMap + (event.channel.id -> Bot.allCommands.toList.toSet)
      }
      _ <- event.sendMessage(s"Enabled all commands.").run
      _ <- Persistence.saveEnabledCommands
    } yield true

  override val description: String = "Disables all commands that can be disabled"
}
