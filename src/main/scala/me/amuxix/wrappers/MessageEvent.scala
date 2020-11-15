package me.amuxix.wrappers

import java.io.File

import me.amuxix.Action
import net.dv8tion.jda.api.JDA
import net.dv8tion.jda.api.entities.{MessageChannel, Member => JDAMember, Message => JDAMessage, User => JDAUser}
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

  def sendMessage(string: String): Action[Message] = channel.sendMessage(string)
  def sendFile(file: File): Action[Message] = channel.sendFile(file)
  lazy val content: String = message.content
}

object MessageEvent {

  implicit def fromMessageReceivedEvent(event: MessageReceivedEvent): MessageEvent =
    new MessageEvent(event.getMessage, event.getChannel, event.getAuthor, Option(event.getMember))

  implicit def fromPrivateMessageReceivedEvent(event: PrivateMessageReceivedEvent): MessageEvent =
    new MessageEvent(event.getMessage, event.getChannel, event.getAuthor, None)

  implicit def fromGuildMessageReceivedEvent(event: GuildMessageReceivedEvent): MessageEvent =
    new MessageEvent(event.getMessage, event.getChannel, event.getAuthor, Option(event.getMember))
}
