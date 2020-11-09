package me.amuxix.commands

import cats.data.NonEmptyList
import cats.effect.IO
import me.amuxix.Bot
import me.amuxix.wrappers.MessageEvent

import scala.util.matching.Regex

object Help extends Command {
  override def regex: Regex = "^help$".r

  private def bold(string: String): String = s"**$string**"

  private def commandHelp(enabledCommands: NonEmptyList[Command], command: Command): String = {
    val name = if (enabledCommands.toList.contains(command)) bold(command.className) else command.className
    s"$name(`${command.regex}`) - ${command.description}"
  }

  override protected def apply(regex: Regex, event: MessageEvent): IO[Boolean] =
    for {
      enabledCommands <- Bot.enabledCommands(event.channel)
      enabledCommandHelp = commandHelp(enabledCommands, _)
      message = Bot.allCommands.filter(!_.isInstanceOf[Hidden]).map(enabledCommandHelp).mkString("\n")
      _ <- event.sendMessage(message)
    } yield true

  override val description: String = "Show all existing commands and their descriptions"
}
