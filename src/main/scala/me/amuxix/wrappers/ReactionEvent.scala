package me.amuxix.wrappers

import cats.effect.IO
import me.amuxix.syntax.all.*
import net.dv8tion.jda.api.entities.{MessageChannel, Member as JDAMember, User as JDAUser, Guild as JDAGuild}
import net.dv8tion.jda.api.events.message.guild.react.GuildMessageReactionAddEvent
import net.dv8tion.jda.api.events.message.priv.react.PrivateMessageReactionAddEvent

class ReactionEvent(
  val emoji: String,
  jdaChannel: MessageChannel,
  jdaAuthor: JDAUser,
  jdaMember: Option[JDAMember],
  messageID: Long,
  jdaGuild: Option[JDAGuild],
) extends Event(jdaChannel, jdaAuthor, jdaMember, jdaGuild):

  lazy val content: String = emoji

  def addReaction(emoji: String): IO[Unit] =
    for
      message <- jdaChannel.retrieveMessageById(messageID).toIO
      _ <- message.addReaction(emoji).toIO
    yield ()

object ReactionEvent:

  implicit def fromGuildMessageReactionAddEvent(event: GuildMessageReactionAddEvent): ReactionEvent =
    new ReactionEvent(
      event.getReactionEmote.getEmoji,
      event.getChannel,
      event.getUser,
      Option(event.getMember),
      event.getMessageIdLong,
      Option(event.getGuild),
    )

  implicit def fromPrivateMessageReactionAddEvent(event: PrivateMessageReactionAddEvent): ReactionEvent =
    new ReactionEvent(
      event.getReactionEmote.getEmoji,
      event.getChannel,
      event.getUser,
      None,
      event.getMessageIdLong,
      None,
    )
