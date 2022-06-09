package me.amuxix.commands

import cats.effect.IO
import me.amuxix.Bot
import me.amuxix.syntax.all.*
import me.amuxix.Bot.userMap
import me.amuxix.commands.slash.SlashPattern
import me.amuxix.wrappers.User
import me.amuxix.wrappers.event.{MessageEvent, SlashCommandEvent}

import scala.util.matching.Regex

object Spam extends SlashCommand with Options:
  override def command: String = "spam"

  override def options: List[SlashPattern => SlashPattern] = List(
    _.addOption[User]("user", "User to toggle the mute")
  )

  override protected def apply(pattern: SlashPattern, event: SlashCommandEvent): IO[Boolean] =
    val user = event.getOption[User]("user")
    if !user.isBot then
      event.reply(s"Muting ${user.name}") *> Bot.spamList.update(_ + user.id).as(true)
    else
      event.reply(s"Can't spam bots").as(false)

  override val description: String = "Spams the mentioned person until a response from the spam pms is given."
