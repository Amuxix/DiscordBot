package me.amuxix.secrethitler.commands

import cats.effect.IO
import me.amuxix.{AnyCommand, Bot}
import me.amuxix.commands.{Hidden, ReactionCommand, TextCommand}
import me.amuxix.secrethitler.Game
import me.amuxix.wrappers.{MessageEvent, ReactionEvent}

import scala.util.matching.Regex

trait SecretHitlerCommand extends Hidden

trait SecretHitlerTextCommand extends TextCommand with SecretHitlerCommand:
  protected def secretHitlerCommand(regex: Regex, event: MessageEvent, game: Game): IO[Option[Game]]

  override protected def apply(regex: Regex, event: MessageEvent): IO[Boolean] =
    for
      game <- Bot.secretHitler.get
      player = event.author
      executed <- game
        .filter { game =>
          val filter = (this == JoinGame || game.isPlaying(player)) &&
            (game.availableCommands(event.author).asInstanceOf[Set[AnyCommand]] + DeleteSecretHitler).contains(this)
          filter
        }
        .fold(IO.pure(false)) { game =>
          for
            updatedGame <- secretHitlerCommand(regex, event, game)
            _ <- Bot.secretHitler.set(updatedGame)
          yield true
        }
    yield executed

  override val description: String = ""

trait SecretHitlerReactionCommand extends ReactionCommand with SecretHitlerCommand:
  protected def secretHitlerCommand(emoji: String, event: ReactionEvent, game: Game): IO[Option[Game]]

  override protected def apply(emoji: String, event: ReactionEvent): IO[Boolean] =
    for
      game <- Bot.secretHitler.get
      player = event.author
      executed <- game
        .filter(game => game.isPlaying(player) && game.availableCommands(event.author).contains(this))
        .fold(IO.pure(false)) { game =>
          for
            updatedGame <- secretHitlerCommand(emoji, event, game)
            _ <- Bot.secretHitler.set(updatedGame)
          yield true
        }
    yield executed

  override val description: String = ""
