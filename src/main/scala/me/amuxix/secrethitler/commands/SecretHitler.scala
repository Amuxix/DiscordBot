package me.amuxix.secrethitler.commands

import cats.effect.IO
import cats.instances.list.*
import cats.syntax.foldable.*
import me.amuxix.Bot
import me.amuxix.commands.TextCommand
import me.amuxix.secrethitler.CreatedGame
import me.amuxix.wrappers.MessageEvent

import scala.util.matching.Regex

object SecretHitler extends TextCommand:
  override def pattern: Regex = "^create secret hitler$".r

  private lazy val message =
    "You have been added to a game of Secret Hitler! You can leave by typing `leave game` in a channel I can read."

  override protected def apply(regex: Regex, event: MessageEvent): IO[Boolean] =
    val author = event.author
    val players = author.voiceChannel.fold(List(author))(_.members.map(_.user))
    val game = new CreatedGame(players.toSet, event.channel, event.guild.get)
    val playersMessage =
      if players.nonEmpty then s" Current players: ${players.map(_.getNameIn(event.guild.get)).mkString(", ")}." else ""
    for
      _ <- event.sendMessage(s"Secret Hitler game created!$playersMessage")
      _ <- players.traverse_(_.sendMessage(message))
      _ <- Bot.secretHitler.set(Some(game)).as(true)
    yield true

  override val description: String =
    "Allows the creation of a secret hitler games as well as all the required commands."
