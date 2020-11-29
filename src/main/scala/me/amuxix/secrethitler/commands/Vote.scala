package me.amuxix.secrethitler.commands

import cats.data.NonEmptyList
import cats.effect.IO
import me.amuxix.AnyCommand
import me.amuxix.secrethitler.{Game, GameDuringElection, GameWithVetoRequest, Vote => GameVote}
import me.amuxix.wrappers.{Event, MessageEvent, ReactionEvent}

import scala.util.matching.Regex

object Vote {

  def nein[E <: Event](event: E, game: Game): IO[Option[Game]] =
    game match {
      case game: GameDuringElection  => game.vote(event.author, GameVote.Nein)
      case game: GameWithVetoRequest => game.deny
      case _                         => event.author.sendMessage("Election not occurring.").as(Some(game))
    }

  def ja[E <: Event](event: E, game: Game): IO[Option[Game]] =
    game match {
      case game: GameDuringElection =>
        game.vote(event.author, GameVote.Ja)
      case game: GameWithVetoRequest => game.accept
      case _                         => event.author.sendMessage("Election not occurring.").as(Some(game))
    }

  lazy val all: NonEmptyList[AnyCommand with SecretHitlerCommand] = NonEmptyList.of(
    JaText,
    JaReaction,
    NeinText,
    NeinReaction,
  )
}

object JaText extends SecretHitlerTextCommand {
  override def pattern: Regex = "^[Jj]a!?$".r

  override protected def secretHitlerCommand(regex: Regex, event: MessageEvent, game: Game): IO[Option[Game]] =
    Vote.ja(event, game)
}

object JaReaction extends SecretHitlerReactionCommand {
  override def pattern: String = "✅"

  override protected def secretHitlerCommand(emoji: String, event: ReactionEvent, game: Game): IO[Option[Game]] =
    Vote.ja(event, game)
}

object NeinText extends SecretHitlerTextCommand {
  override def pattern: Regex = "^[Nn]ein!?$".r

  override protected def secretHitlerCommand(regex: Regex, event: MessageEvent, game: Game): IO[Option[Game]] =
    Vote.nein(event, game)
}

object NeinReaction extends SecretHitlerReactionCommand {
  override def pattern: String = "❎"

  override protected def secretHitlerCommand(emoji: String, event: ReactionEvent, game: Game): IO[Option[Game]] =
    Vote.nein(event, game)
}
