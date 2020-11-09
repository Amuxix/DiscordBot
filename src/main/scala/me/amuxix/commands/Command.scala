package me.amuxix.commands

import cats.effect.IO
import me.amuxix.{Bot, Named}
import me.amuxix.wrappers.MessageEvent

import scala.util.matching.Regex

abstract class Command extends Named {
  def regex: Regex
  protected def apply(regex: Regex, event: MessageEvent): IO[Boolean]

  def run(regex: Regex, event: MessageEvent): IO[Boolean] =
    for {
      roles <- Bot.allowedRoles.get
      allowed = event.authorMember.fold(false) { member =>
        member.isGuildOwner || (roles & member.roles.map(_.id)).nonEmpty || member.id == 211184778815340544L
      }
      stop <- if (allowed) apply(regex, event) else IO.pure(false)
    } yield stop

  val description: String

  override def toString: String = className
}

object Command {
  val all = ".+".r
}
