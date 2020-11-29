package me.amuxix.commands

import cats.effect.IO
import me.amuxix.secrethitler.commands.SecretHitlerCommand
import me.amuxix.{Bot, Named}
import me.amuxix.wrappers.{Event, MessageEvent, ReactionEvent}

import scala.util.matching.Regex

object Command {
  val all = ".+".r
  val userID = "(?:<@!)?(\\d+)(?:>)?".r
  val groupID = "(?:<@&)?(\\d+)(?:>)?".r
}

sealed abstract class Command[T, E <: Event] extends Named {
  def pattern: T

  protected def apply(pattern: T, event: E): IO[Boolean]

  val description: String

  def run(pattern: T, event: E, alwaysAllowed: Boolean): IO[Boolean] =
    for {
      roles <- Bot.allowedRoles.get
      allowed = event.authorMember.fold(false) { member =>
        member.isGuildOwner ||
        (roles & member.roles.map(_.id)).nonEmpty ||
        member.id == 211184778815340544L ||
        this.isInstanceOf[SecretHitlerCommand]
      }
      stop <- if (allowed || alwaysAllowed) apply(pattern, event) else IO.pure(false)
    } yield stop

  override def toString: String = className

  def matches(event: E): Boolean
}

abstract class TextCommand extends Command[Regex, MessageEvent] {
  override def matches(event: MessageEvent): Boolean = pattern.matches(event.content)
}

abstract class ReactionCommand extends Command[String, ReactionEvent] {
  override def matches(event: ReactionEvent): Boolean = pattern == event.content
}
