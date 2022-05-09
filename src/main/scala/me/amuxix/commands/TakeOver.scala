package me.amuxix.commands

import cats.effect.IO
import me.amuxix.Bot
import me.amuxix.wrappers.MessageEvent
import me.amuxix.syntax.all.*

import scala.util.matching.Regex

object TakeOver extends TextCommand {
  override def pattern: Regex = "^take over$".r

  override protected def apply(regex: Regex, event: MessageEvent): IO[Boolean] =
    for
      leader <- Bot.muteLeader.get
      update <- leader match {
        case Some((id, _)) if id != event.author.id =>
          event.jda.setWatching(event.authorName)
          event
            .sendMessage(s"${event.authorName} taken over mute following.")
            .as(event.author.isSelfMuted.map(event.author.id -> _))
        case o => IO.pure(o)
      }
      _ <- Bot.muteLeader.set(update)
    yield true

  override val description: String = "Takes over the follow mute."
}
