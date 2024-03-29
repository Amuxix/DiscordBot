package me.amuxix.commands

import cats.effect.IO
import me.amuxix.Bot
import me.amuxix.wrappers.event.MessageEvent

import scala.util.matching.Regex

object StopSpam extends TextCommand with Hidden:
  override def pattern: Regex = Command.all

  override protected def apply(regex: Regex, event: MessageEvent): IO[Boolean] =
    for
      list <- Bot.spamList.get
      author = event.author
      id = author.id
      stop <-
        if list.contains(id) then
          IO.println(s"Stopped spamming ${author.name}") *> Bot.spamList.update(_ - id).as(true)
        else
          IO.pure(false)
    yield stop

  override val description: String = "Stops the spam messages"
