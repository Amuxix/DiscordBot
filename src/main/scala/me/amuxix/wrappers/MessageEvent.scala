package me.amuxix.wrappers

import net.dv8tion.jda.api.entities.{MessageChannel, Member => JDAMember, Message => JDAMessage, User => JDAUser, Guild => JDAGuild}
import net.dv8tion.jda.api.events.message.guild.GuildMessageReceivedEvent
import net.dv8tion.jda.api.events.message.priv.PrivateMessageReceivedEvent

class MessageEvent(
  jdaMessage: JDAMessage,
  jdaChannel: MessageChannel,
  jdaAuthor: JDAUser,
  jdaMember: Option[JDAMember],
  jdaGuild: Option[JDAGuild],
) extends Event(jdaChannel, jdaAuthor, jdaMember, jdaGuild) {
  lazy val message: Message = new Message(jdaMessage)
  lazy val content: String = message.content
  lazy val authorName = jdaMember.map(_.getNickname).getOrElse(author.name)
}

object MessageEvent {

  implicit def fromPrivateMessageReceivedEvent(event: PrivateMessageReceivedEvent): MessageEvent =
    new MessageEvent(event.getMessage, event.getChannel, event.getAuthor, None, None)

  implicit def fromGuildMessageReceivedEvent(event: GuildMessageReceivedEvent): MessageEvent =
    new MessageEvent(
      event.getMessage,
      event.getChannel,
      event.getAuthor,
      Option(event.getMember),
      Option(event.getGuild),
    )
}
