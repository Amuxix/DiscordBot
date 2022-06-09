package me.amuxix.commands

import cats.effect.IO
import me.amuxix.Bot
import me.amuxix.wrappers.event.MessageEvent

import scala.util.matching.Regex

object ListEnabledCommands extends TextCommand with Hidden:
  override def pattern: Regex = "^list enabled commands$".r

  override protected def apply(regex: Regex, event: MessageEvent): IO[Boolean] =
    for
      enabledCommands <- Bot.enabledCommands.get
      channelCommands = enabledCommands.getOrElse(event.channel.id, Set.empty)
      _ <- event.reply(s"Enabled commands: ${channelCommands.mkString(", ")}")
    yield true

  override val description: String = "Lists all enabled commands."
