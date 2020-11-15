package me.amuxix.commands

import cats.data.NonEmptyList
import cats.effect.IO
import me.amuxix.Bot
import me.amuxix.wrappers.MessageEvent
import me.amuxix.syntax.all._

import scala.util.matching.Regex

object Help extends Command {
  override def regex: Regex = "^help$".r

  implicit private class StringOps(string: String) {
    def bold: String = s"**$string**"
    def underline: String = s"__${string}__"
  }

  private def commandHelp(enabledCommands: NonEmptyList[Command], command: Command): String = {
    val name = command.className
      .when(enabledCommands.toList.contains(command))(_.bold)
      .when(Bot.alwaysEnabled.toList.contains(command))(_.underline)

    s"$name(`${command.regex}`) - ${command.description}"
  }

  override protected def apply(regex: Regex, event: MessageEvent): IO[Boolean] =
    for {
      enabledCommands <- Bot.enabledCommands(event.channel)
      enabledCommandHelp = commandHelp(enabledCommands, _)
      message = Bot.allCommands.filter(!_.isInstanceOf[Hidden]).map(enabledCommandHelp).mkString("\n")
      _ <- event.sendMessage(message).run
    } yield true

  override val description: String = "Show all existing commands and their descriptions"
}
