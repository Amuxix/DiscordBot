package me.amuxix.commands

import cats.effect.IO
import me.amuxix.secrethitler.commands.SecretHitler
import me.amuxix.wrappers.MessageEvent
import me.amuxix.{Bot, Persistence}

import scala.util.matching.Regex

object EnableCommand extends TextCommand {
  override def pattern: Regex = "^enable (.+)$".r

  override protected def apply(regex: Regex, event: MessageEvent): IO[Boolean] =
    event.content match {
      case regex(commandName) =>
        for {
          enabledCommands <- Bot.enabledCommands(event.channel)
          command = Bot.allCommands
            .find(_.className.equalsIgnoreCase(commandName))
            .filterNot(enabledCommands.toList.contains)
          noCommand = event.sendMessage(s"No command named `$commandName` found!").as(())
          _ <- command.fold(noCommand) {
            case command if command == SecretHitler =>
              for {
                _ <- Bot.enabledCommands.update { enabledCommandsMap =>
                  val enabledCommands = enabledCommandsMap.getOrElse(event.channel.id, Set.empty)
                  enabledCommandsMap + (event.channel.id -> (enabledCommands ++ Bot.secretHitlerCommands.toList))
                }
                _ <- event.sendMessage(s"Enabled all secret hitler commands.")
                _ <- Persistence.saveEnabledCommands
              } yield ()
            case command =>
              for {
                _ <- Bot.enabledCommands.update { enabledCommandsMap =>
                  val enabledCommands = enabledCommandsMap.getOrElse(event.channel.id, Set.empty)
                  enabledCommandsMap + (event.channel.id -> (enabledCommands + command))
                }
                _ <- event.sendMessage(s"Enabled `${command.className}` command.")
                _ <- Persistence.saveEnabledCommands
              } yield ()
          }
        } yield true
    }

  override val description: String = "Enables the given command"
}
