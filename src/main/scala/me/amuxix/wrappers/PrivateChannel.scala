package me.amuxix.wrappers

import cats.effect.IO
import me.amuxix.syntax.action.*
import net.dv8tion.jda.api.entities.PrivateChannel as JDAPrivateChannel

class PrivateChannel(privateChannel: JDAPrivateChannel) {
  def sendMessage(message: String): IO[Message] = privateChannel.sendMessage(message).toIO.map(new Message(_))
}
