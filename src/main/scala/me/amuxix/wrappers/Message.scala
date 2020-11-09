package me.amuxix.wrappers

import cats.effect.IO
import net.dv8tion.jda.api.entities.{Message => JDAMessage}
import me.amuxix.Implicits._

class Message(message: JDAMessage) {
  lazy val content: String = message.getContentRaw
  def addReaction(string: String): IO[Unit] = message.addReaction(string).run
  def edit(string: String): IO[Unit] = message.editMessage(string).run
}
