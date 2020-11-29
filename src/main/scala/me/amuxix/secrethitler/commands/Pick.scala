package me.amuxix.secrethitler.commands

import cats.effect.IO
import me.amuxix.secrethitler.{FascistPolicy, Game, GameWaitingForChancellorDecision, GameWithChancellorElected, LiberalPolicy}
import me.amuxix.wrappers.ReactionEvent

object PickFascist extends SecretHitlerReactionCommand {
  override def pattern: String = FascistPolicy.emote

  override protected def secretHitlerCommand(emoji: String, event: ReactionEvent, game: Game): IO[Option[Game]] =
    game match {
      case game: GameWithChancellorElected        => game.discardPolicy(FascistPolicy)
      case game: GameWaitingForChancellorDecision => game.enactPolicy(FascistPolicy)
      case _                                      => IO.pure(Some(game))
    }
}

object PickLiberal extends SecretHitlerReactionCommand {
  override def pattern: String = LiberalPolicy.emote

  override protected def secretHitlerCommand(emoji: String, event: ReactionEvent, game: Game): IO[Option[Game]] =
    game match {
      case game: GameWithChancellorElected        => game.discardPolicy(LiberalPolicy)
      case game: GameWaitingForChancellorDecision => game.enactPolicy(LiberalPolicy)
      case _                                      => IO.pure(Some(game))
    }
}
