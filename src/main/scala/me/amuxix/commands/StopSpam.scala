package me.amuxix.commands

import cats.effect.IO
import me.amuxix.Bot
import me.amuxix.wrappers.MessageEvent

import scala.util.matching.Regex

object StopSpam extends Command with Hidden {
  override def regex: Regex = Command.all

  override protected def apply(regex: Regex, event: MessageEvent): IO[Boolean] =
    for {
      list <- Bot.spamList.get
      author = event.author
      id = author.id
      stop <-
        if (list.contains(id)) {
          IO(println(s"Stopped spamming ${author.name}")) *> Bot.spamList.update(_ - id).as(true)
        } else {
          IO.pure(false)
        }
    } yield stop

  override val description: String = "Stops the spam messages"
}
