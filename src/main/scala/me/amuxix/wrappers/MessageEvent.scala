package me.amuxix.wrappers

import java.io.File

import cats.effect.IO
import net.dv8tion.jda.api.JDA
import net.dv8tion.jda.api.entities.{MessageChannel, Message => JDAMessage, User => JDAUser, Member => JDAMember}
import net.dv8tion.jda.api.events.message.MessageReceivedEvent
import net.dv8tion.jda.api.events.message.guild.GuildMessageReceivedEvent
import net.dv8tion.jda.api.events.message.priv.PrivateMessageReceivedEvent

class MessageEvent(
  jdaMessage: JDAMessage,
  jdaChannel: MessageChannel,
  jdaAuthor: JDAUser,
  jdaMember: Option[JDAMember],
) {
  lazy val message: Message = new Message(jdaMessage)
  lazy val channel: Channel = new Channel(jdaChannel)
  lazy val author: User = new User(jdaAuthor)
  lazy val authorMember: Option[Member] = jdaMember.map(new Member(_))

  lazy val fromBot: Boolean = author.isBot

  lazy val jda: JDA = jdaMessage.getJDA

  def sendMessage(string: String): IO[Unit] = channel.sendMessage(string)
  def sendFile(file: File): IO[Unit] = channel.sendFile(file)
  lazy val content: String = message.content
}

object MessageEvent {
  implicit def fromMessageReceivedEvent(event: MessageReceivedEvent): MessageEvent =
    new MessageEvent(event.getMessage, event.getChannel, event.getAuthor, Some(event.getMember))

  implicit def fromPrivateMessageReceivedEvent(event: PrivateMessageReceivedEvent): MessageEvent =
    new MessageEvent(event.getMessage, event.getChannel, event.getAuthor, None)

  implicit def fromGuildMessageReceivedEvent(event: GuildMessageReceivedEvent): MessageEvent =
    new MessageEvent(event.getMessage, event.getChannel, event.getAuthor, Some(event.getMember))
}
