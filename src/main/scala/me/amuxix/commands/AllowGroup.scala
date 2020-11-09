package me.amuxix.commands
import cats.effect.IO
import me.amuxix.{Bot, Persistence}
import me.amuxix.Bot.roleMap
import me.amuxix.Implicits._
import me.amuxix.wrappers.MessageEvent

import scala.util.matching.Regex

object AllowGroup extends Command {
  override def regex: Regex = "^allow (group|role) <@&(\\d+)>$".r

  override protected def apply(regex: Regex, event: MessageEvent): IO[Boolean] =
    event.content match {
      case regex(id) =>
        event.jda.getRoleByID(id.toLong).flatMap {
          case None => IO.pure(false)
          case Some(role) =>
            for {
              _ <- Bot.allowedRoles.update(_ + role.id)
              _ <- event.sendMessage(s"Users of ${role.name} can now use this bot.")
              _ <- Persistence.saveAllowedRoles
            } yield true
        }
      case _ => IO.pure(false)
    }

  override val description: String = "Allows users from the given group to use this bot."
}
