package me.amuxix.wrappers

import cats.effect.IO
import me.amuxix.syntax.action._
import net.dv8tion.jda.api.entities.{PrivateChannel => JDAPrivateChannel}

class PrivateChannel(privateChannel: JDAPrivateChannel) {
  def sendMessage(message: String): IO[Message] = privateChannel.sendMessage(message).toIO.map(new Message(_))
}
