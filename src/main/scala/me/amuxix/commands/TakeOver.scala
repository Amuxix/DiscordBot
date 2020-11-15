package me.amuxix.commands

import cats.effect.IO
import me.amuxix.Bot
import me.amuxix.wrappers.MessageEvent
import me.amuxix.syntax.all._

import scala.util.matching.Regex

object TakeOver extends Command {
  override def regex: Regex = "^take over$".r

  override protected def apply(regex: Regex, event: MessageEvent): IO[Boolean] =
    for {
      leader <- Bot.muteLeader.get
      update <- leader match {
        case Some((id, _)) if id != event.author.id =>
          event.jda.setWatching(event.author.name)
          event
            .sendMessage(s"${event.author.name} taken over mute following.")
            .run
            .as(event.author.isSelfMuted.map(event.author.id -> _))
        case o => IO.pure(o)
      }
      _ <- Bot.muteLeader.set(update)
    } yield true

  override val description: String = "Takes over the follow mute."
}
