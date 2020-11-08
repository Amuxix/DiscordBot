package me.amuxix.wrappers

import cats.effect.IO
import net.dv8tion.jda.api.entities.{PrivateChannel, User => JDAUser}

import scala.jdk.CollectionConverters._

class User(user: JDAUser) {
  lazy val id: Long = user.getIdLong
  lazy val name: String = user.getName
  lazy val isBot: Boolean = user.isBot

  def openPrivateChannel(f: PrivateChannel => Unit): IO[Unit] = IO(user.openPrivateChannel().queue(f(_)))

  def voiceChannel: Option[VoiceChannel] = user.getJDA.getVoiceChannels.asScala.toList.collectFirst {
    case channel if channel.getMembers.asScala.exists(_.getUser.getIdLong == user.getIdLong) => new VoiceChannel(channel)
  }

  def member: Option[Member] = voiceChannel.flatMap(_.members.find(_.id == id))

  def isSelfMuted: Option[Boolean] = member.map(_.isSelfMuted)
}
