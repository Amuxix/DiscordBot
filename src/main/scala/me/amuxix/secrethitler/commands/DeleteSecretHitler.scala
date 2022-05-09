package me.amuxix.secrethitler.commands

import cats.effect.IO
import me.amuxix.Bot
import me.amuxix.commands.TextCommand
import me.amuxix.wrappers.MessageEvent

import scala.util.matching.Regex

object DeleteSecretHitler extends TextCommand:
  override def pattern: Regex = "^delete secret hitler$".r

  override protected def apply(pattern: Regex, event: MessageEvent): IO[Boolean] =
    for
      game <- Bot.secretHitler.get
      message = game.fold("No game running!")(_ => "Game deleted")
      _ <- Bot.secretHitler.set(None)
      _ <- event.sendMessage(message)
    yield true

  override val description: String = "Allows the deletion of a secret hitler game."
