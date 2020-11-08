package me.amuxix.wrappers

import cats.effect.IO
import net.dv8tion.jda.api.entities.{VoiceChannel => JDAVoiceChannel}
import scala.jdk.CollectionConverters._
import cats.instances.list._
import cats.syntax.foldable._

class VoiceChannel(channel: JDAVoiceChannel) {
  def members: List[Member] = channel.getMembers.asScala.toList.map(new Member(_))

  def toggleMuteAll: IO[Unit] = members.traverse_(_.toggleMute)
}
