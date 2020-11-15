package me.amuxix.wrappers

import net.dv8tion.jda.api.entities.{VoiceChannel => JDAVoiceChannel}

import scala.jdk.CollectionConverters._
import me.amuxix.Action

class VoiceChannel(channel: JDAVoiceChannel) {
  def members: List[Member] = channel.getMembers.asScala.toList.map(new Member(_))

  def toggleMuteAll: List[Action[Unit]] = members.map(_.toggleMute)
  def muteAll: List[Action[Unit]] = members.map(_.mute)
  def unmuteAll: List[Action[Unit]] = members.map(_.unmute)
}
