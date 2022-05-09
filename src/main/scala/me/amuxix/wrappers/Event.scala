package me.amuxix.wrappers

import java.io.File

import cats.effect.IO
import net.dv8tion.jda.api.JDA
import net.dv8tion.jda.api.entities.{MessageChannel, Member as JDAMember, User as JDAUser, Guild as JDAGuild}

abstract class Event(
  jdaChannel: MessageChannel,
  jdaAuthor: JDAUser,
  jdaMember: Option[JDAMember],
  jdaGuild: Option[JDAGuild],
) {
  lazy val author: User = new User(jdaAuthor)
  lazy val authorMember: Option[Member] = jdaMember.map(new Member(_)).orElse(author.member)
  lazy val channel: Channel = new Channel(jdaChannel)
  lazy val fromBot: Boolean = author.isBot
  lazy val guild: Option[Guild] = jdaGuild.map(new Guild(_))
  lazy val jda: JDA = jdaAuthor.getJDA

  def sendMessage(string: String): IO[Message] = channel.sendMessage(string)

  def sendFile(file: File): IO[Message] = channel.sendFile(file)

  def content: String
}
