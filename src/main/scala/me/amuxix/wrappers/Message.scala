package me.amuxix.wrappers

import cats.effect.IO
import me.amuxix.syntax.action._
import cats.instances.list._
import cats.syntax.foldable._
import net.dv8tion.jda.api.entities.{Message => JDAMessage}

class Message(message: JDAMessage) {
  lazy val content: String = message.getContentRaw

  def addReactions(reactions: String*): IO[Unit] =
    reactions.toList.traverse_(reaction => message.addReaction(reaction).toIO)
  def addReaction(string: String): IO[Unit] = message.addReaction(string).toIO.as(())
  def edit(string: String): IO[Message] = message.editMessage(string).toIO.map(new Message(_))
}
