package me.amuxix.wrappers

import java.net.URL

import cats.effect.IO
import me.amuxix.Bot
import net.dv8tion.jda.api.entities.{PrivateChannel, TextChannel, User => JDAUser}

import scala.concurrent.Promise
import scala.jdk.CollectionConverters._
import scala.util.Success

class User(user: JDAUser) {
  lazy val id: Long = user.getIdLong
  lazy val name: String = user.getName
  lazy val avatarURL: URL = new URL(user.getAvatarUrl)
  lazy val isBot: Boolean = user.isBot
  private lazy val promise = {
    val p = Promise[PrivateChannel]()
    user.openPrivateChannel().queue(channel => p.complete(Success(channel)))
    p
  }
  val privateChannel: IO[PrivateChannel] = IO.fromFuture(IO(promise.future))(Bot.cs)

  //def groups(guild: Guild, channel: TextChannel) = channel.getMembers.asScala.toList.map(_.getRoles)

  def openPrivateChannel(f: PrivateChannel => Unit): IO[Unit] = IO(user.openPrivateChannel().queue(f(_)))

  def voiceChannel: Option[VoiceChannel] = user.getJDA.getVoiceChannels.asScala.toList.collectFirst {
    case channel if channel.getMembers.asScala.exists(_.getUser.getIdLong == user.getIdLong) =>
      new VoiceChannel(channel)
  }

  def member: Option[Member] = voiceChannel.flatMap(_.members.find(_.id == id))

  def isSelfMuted: Option[Boolean] = member.map(_.isSelfMuted)

  override def toString: String = s"$name($id)"
}
