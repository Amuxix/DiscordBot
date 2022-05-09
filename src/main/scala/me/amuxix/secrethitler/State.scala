package me.amuxix.secrethitler

import cats.effect.IO
import me.amuxix.Bot
import me.amuxix.commands.TextCommand
import me.amuxix.wrappers.MessageEvent

import scala.util.matching.Regex

object State extends TextCommand:
  override def pattern: Regex = "^get game state$".r

  override protected def apply(pattern: Regex, event: MessageEvent): IO[Boolean] =
    for
      game <- Bot.secretHitler.get
      _ <- event.sendMessage(game.map(_.getClass.getSimpleName).getOrElse("")).as(Some(game))
    yield true

  override val description: String = ""
