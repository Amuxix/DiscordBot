package me.amuxix.wrappers

import java.io.File

import cats.effect.IO
import net.dv8tion.jda.api.entities.MessageChannel
import me.amuxix.Implicits._

class Channel(channel: MessageChannel) {
  def sendMessage(string: String): IO[Unit] = channel.sendMessage(string).run
  def sendFile(file: File): IO[Unit] = channel.sendFile(file).run
}
