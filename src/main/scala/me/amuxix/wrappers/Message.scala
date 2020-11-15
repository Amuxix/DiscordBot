package me.amuxix.wrappers

import me.amuxix.Action
import net.dv8tion.jda.api.entities.{Message => JDAMessage}

class Message(message: JDAMessage) {
  lazy val content: String = message.getContentRaw
  def addReaction(string: String): Action[Unit] = new Action(message.addReaction(string)).as(())
  def edit(string: String): Action[Message] = new Action(message.editMessage(string).map(new Message(_)))
}
