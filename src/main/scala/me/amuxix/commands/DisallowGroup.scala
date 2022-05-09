package me.amuxix.commands

import cats.effect.IO
import me.amuxix.{Bot, Persistence}
import me.amuxix.Bot.roleMap
import me.amuxix.syntax.all.*
import me.amuxix.wrappers.MessageEvent

import scala.util.matching.Regex

object DisallowGroup extends TextCommand {
  override def pattern: Regex = s"^allow (?:group|role) ${Command.groupID}$$".r

  override protected def apply(regex: Regex, event: MessageEvent): IO[Boolean] =
    event.content match {
      case regex(id) =>
        event.jda.getRoleByID(id.toLong).flatMap {
          case None => IO.pure(false)
          case Some(role) =>
            for
              _ <- Bot.allowedRoles.update(_ - role.id)
              _ <- event.sendMessage(s"Users of ${role.name} can no longer use this bot.")
              _ <- Persistence.saveAllowedRoles
            yield true
        }
      case _ =>
        IO.pure(false)
    }

  override val description: String = "Removes the group from the allowed groups that can use this bot."
}
