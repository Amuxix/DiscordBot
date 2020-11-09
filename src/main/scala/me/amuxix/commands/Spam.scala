package me.amuxix.commands

import cats.effect.IO
import me.amuxix.Bot
import me.amuxix.wrappers.MessageEvent
import me.amuxix.Implicits._
import me.amuxix.Bot.userMap

import scala.util.matching.Regex

object Spam extends Command {
  override def regex: Regex = "[Ss]pam <@!(\\d+)>".r

  override protected def apply(regex: Regex, event: MessageEvent): IO[Boolean] =
    event.content match {
      case regex(id) =>
        event.jda.getUserByID(id.toLong).flatMap {
          case None => IO.pure(false)
          case Some(user) =>
            if (!user.isBot) Bot.spamList.update(_ + user.id).as(true) else IO.pure(false)
        }
      case _ => IO.pure(false)
    }

  override val description: String = "Spams the mentioned person until a response from the spam pms is given."
}
