package me.amuxix.commands

import cats.effect.IO
import me.amuxix.wrappers.event.MessageEvent
import me.amuxix.{Bot, Persistence}

import scala.util.matching.Regex

object EnableCommand extends TextCommand:
  override def pattern: Regex = "^enable (.+)$".r

  override protected def apply(regex: Regex, event: MessageEvent): IO[Boolean] =
    event.content match
      case regex(commandName) =>
        for
          enabledCommands <- Bot.enabledCommands(event.channel)
          command = Bot.allCommands
            .find(_.className.equalsIgnoreCase(commandName))
            .filterNot(enabledCommands.toList.contains)
          noCommand = event.reply(s"No command named `$commandName` found!").as(())
          _ <- command.fold(noCommand) { command =>
            for
              _ <- Bot.enabledCommands.update { enabledCommandsMap =>
                val enabledCommands = enabledCommandsMap.getOrElse(event.channel.id, Set.empty)
                enabledCommandsMap + (event.channel.id -> (enabledCommands + command))
              }
              _ <- event.reply(s"Enabled `${command.className}` command.")
              _ <- Persistence.saveEnabledCommands
            yield ()
          }
        yield true

  override val description: String = "Enables the given command"
