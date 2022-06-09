package me.amuxix.commands

import cats.effect.IO
import me.amuxix.Bot
import me.amuxix.commands.slash.SlashPattern
import me.amuxix.syntax.all.*
import me.amuxix.wrappers.event.{MessageEvent, SlashCommandEvent}

import scala.util.matching.Regex

object TakeOver extends SlashCommand:
  override def command: String = "take_over"

  override protected def apply(pattern: SlashPattern, event: SlashCommandEvent): IO[Boolean] =
    for
      leader <- Bot.muteLeader.get
      update <- leader match
        case Some((id, _)) if id != event.author.id =>
          event.jda.setWatching(event.authorName)
          event
            .reply(s"${event.authorName} taken over mute following.")
            .as(event.author.isSelfMuted.map(event.author.id -> _))
        case o => IO.pure(o)
      _ <- Bot.muteLeader.set(update)
    yield true

  override val description: String = "Takes over the follow mute."
