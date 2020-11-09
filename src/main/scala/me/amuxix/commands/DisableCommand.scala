package me.amuxix.commands

import cats.effect.IO
import me.amuxix.wrappers.MessageEvent
import me.amuxix.{Bot, Persistence}

import scala.util.matching.Regex

object DisableCommand extends Command {
  override def regex: Regex = "^disable (.+)$".r

  override protected def apply(regex: Regex, event: MessageEvent): IO[Boolean] =
    event.content match {
      case regex(commandName) =>
        for {
          enabledCommands <- Bot.enabledCommands(event.channel)
          command = Bot.allCommands
            .find(_.className.equalsIgnoreCase(commandName))
            .filter(enabledCommands.toList.contains)
          _ <- command.fold(IO.unit) { command =>
            for {
              _ <- Bot.enabledCommands.update { enabledCommandsMap =>
                val enabledCommands = enabledCommandsMap.getOrElse(event.channel.id, Set.empty)
                enabledCommandsMap + (event.channel.id -> (enabledCommands - command))
              }
              _ <- event.sendMessage(s"Disabled `${command.className}` command.")
              _ <- Persistence.saveEnabledCommands
            } yield ()
          }
        } yield true
    }

  override val description: String = "Disables the given command"
}
