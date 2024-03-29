package me.amuxix.wrappers

import java.io.File

import cats.effect.IO
import me.amuxix.syntax.action.*
import net.dv8tion.jda.api.entities.MessageChannel

class Channel(channel: MessageChannel):
  lazy val id: Long = channel.getIdLong
  lazy val name: String = channel.getName
  def sendMessage(string: String): IO[Message] = channel.sendMessage(string).toIO.map(new Message(_))
  def sendFile(file: File): IO[Message] = channel.sendFile(file).toIO.map(new Message(_))

  override def toString: String = s"#$name($id)"
