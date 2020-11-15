package me.amuxix.commands

import cats.effect.IO
import me.amuxix.Bot.{muteLeader, userMap}
import me.amuxix.wrappers.MessageEvent
import me.amuxix.syntax.all._

import scala.util.matching.Regex

object FollowMute extends Command {
  override def regex: Regex = "^(?:un)?follow mute$".r

  override protected def apply(regex: Regex, event: MessageEvent): IO[Boolean] =
    for {
      leader <- muteLeader.get
      update <- leader match {
        case Some((id, _)) if id == event.author.id =>
          event.jda.clearActivity()
          event.sendMessage(s"Stopped following ${event.author.name}.").run.as(None)
        case None =>
          event.jda.setWatching(event.author.name)
          event
            .sendMessage(s"Now following ${event.author.name}")
            .run
            .as(event.author.isSelfMuted.map(event.author.id -> _))
        case some @ Some((id, _)) =>
          for {
            following <- event.jda.getUserByID(id)
            _ <- following.fold(IO.unit)(following => event.sendMessage(s"Already following ${following.name}.").run)
          } yield some
      }
      _ <- muteLeader.set(update)
    } yield true

  override val description: String =
    "Will mute everyone in your voice chat until you call this command again or some takes over"
}
