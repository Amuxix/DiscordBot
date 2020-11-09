package me.amuxix.commands

import cats.effect.IO
import cats.syntax.foldable._
import me.amuxix.wrappers.MessageEvent
import me.amuxix.Implicits._
import me.amuxix.Bot.userMap

import scala.util.matching.Regex

object Mute extends Command {
  override def regex: Regex = "^mute <@!(\\d+)>$".r

  override protected def apply(regex: Regex, event: MessageEvent): IO[Boolean] =
    event.content match {
      case regex(id) =>
        event.jda.getUserByID(id.toLong).flatMap {
          case None => IO.pure(false)
          case Some(user) =>
            println(s"Muting ${user.name}")
            if (!user.isBot) user.member.traverse_(_.toggleMute).as(true) else IO.pure(false)
        }
      case _ => IO.pure(false)
    }

  override val description: String = "Mutes the mentioned person."
}
