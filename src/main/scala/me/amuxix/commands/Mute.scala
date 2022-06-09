package me.amuxix.commands

import cats.effect.IO
import cats.syntax.foldable.*
import me.amuxix.syntax.all.*
import me.amuxix.Bot.userMap
import me.amuxix.wrappers.event.MessageEvent

import scala.util.matching.Regex

object Mute extends TextCommand:
  override def pattern: Regex = s"^(?:un)?mute ${Command.userID}$$".r

  override protected def apply(regex: Regex, event: MessageEvent): IO[Boolean] =
    event.content match
      case regex(id) =>
        println(s"Muting $id")
        event.jda.getUserByID(id.toLong).flatMap { user =>
          println(s"Muting ${user.name}")
          if !user.isBot then user.member.traverse_(_.toggleMute).as(true) else IO.pure(false)
        }
      case _ => IO.pure(false)

  override val description: String = "Mutes the mentioned person."
