package me.amuxix.commands

import cats.effect.IO
import cats.syntax.foldable.*
import me.amuxix.syntax.all.*
import me.amuxix.Bot.userMap
import me.amuxix.commands.slash.SlashPattern
import me.amuxix.wrappers.User
import me.amuxix.wrappers.event.{MessageEvent, SlashCommandEvent}

import scala.util.matching.Regex

object Mute extends SlashCommand with Options:
  override def command: String = "mute"

  override def options: List[SlashPattern => SlashPattern] = List(
    _.addOption[User]("user", "User to toggle the mute")
  )

  override protected def apply(pattern: SlashPattern, event: SlashCommandEvent): IO[Boolean] =
    val user = event.getOption[User]("user")
    if !user.isBot then
      user.member.traverse_ { member =>
        val message = if member.isGuildMuted then s"Unmuting ${user.name}" else s"Muting ${user.name}"
        event.reply(message) *> user.member.traverse_(_.toggleMute)
      }.as(true)
    else
      event.reply(s"Can't mute bots").as(false)

  override val description: String = "Toggles the mute on the mentioned person."
