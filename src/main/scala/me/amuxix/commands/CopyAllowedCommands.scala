package me.amuxix.commands

import cats.effect.IO
import me.amuxix.{Bot, Persistence}
import me.amuxix.syntax.all._
import me.amuxix.wrappers.MessageEvent

import scala.util.Try
import scala.util.matching.Regex

object CopyAllowedCommands extends Command {
  override def regex: Regex = "^copy allowed commands (.+)$".r

  override protected def apply(regex: Regex, event: MessageEvent): IO[Boolean] =
    event.content match {
      case regex(id) =>
        Try(id.toLong).toOption
          .fold(event.jda.getChannelByName(id))(event.jda.getChannelByID)
          .flatMap {
            case None => event.sendMessage(s"Could not find channel with provided name or ID.").run
            case Some(channel) =>
              for {
                _ <- Bot.enabledCommands.update { enabledCommands =>
                  val option = enabledCommands.get(channel.id)
                  enabledCommands + option.fold(event.channel.id -> Set.empty[Command])(event.channel.id -> _)
                }
                _ <- event.sendMessage(s"Copy successful.").run
                _ <- Persistence.saveEnabledCommands
              } yield ()
          }
          .as(true)
      case _ => IO.pure(false)
    }

  override val description: String = "Copies the allowed commands from the given channel id or name."
}
