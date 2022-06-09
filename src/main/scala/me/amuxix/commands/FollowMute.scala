package me.amuxix.commands

import cats.effect.IO
import me.amuxix.Bot.{muteLeader, userMap}
import me.amuxix.commands.slash.SlashPattern
import me.amuxix.syntax.all.*
import me.amuxix.wrappers.event.{MessageEvent, SlashCommandEvent}

import scala.util.matching.Regex

object FollowMute extends SlashCommand:
  override def command: String = "follow_mute"

  override protected def apply(pattern: SlashPattern, event: SlashCommandEvent): IO[Boolean] =
    for
      leader <- muteLeader.get
      update <- leader match
        case Some((id, _)) if id == event.author.id =>
          event.jda.clearActivity()
          event.reply(s"Stopped following ${event.authorName}.").as(None)
        case None =>
          event.jda.setWatching(event.authorName)
          event.reply(s"Now following ${event.authorName}").as(event.author.isSelfMuted.map(event.author.id -> _))
        case some @ Some((id, _)) =>
          for
            following <- event.jda.getUserByID(id)
            _ <- event.reply(s"Already following ${following.name}.")
          yield some
      _ <- muteLeader.set(update)
    yield true

  override val description: String =
    "Will mute everyone in your voice chat until you call this command again or some takes over"
