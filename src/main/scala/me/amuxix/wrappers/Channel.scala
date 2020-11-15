package me.amuxix.wrappers

import java.io.File

import me.amuxix.Action
import me.amuxix.syntax.action._
import net.dv8tion.jda.api.entities.MessageChannel

class Channel(channel: MessageChannel) {
  lazy val id: Long = channel.getIdLong
  lazy val name: String = channel.getName
  def sendMessage(string: String): Action[Message] = channel.sendMessage(string).map(new Message(_))
  def sendFile(file: File): Action[Message] = channel.sendFile(file).map(new Message(_))

  override def toString: String = s"#$name($id)"
}
