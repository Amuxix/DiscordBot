package me.amuxix.wrappers

import cats.effect.IO
import me.amuxix.syntax.action._
import net.dv8tion.jda.api.Permission
import net.dv8tion.jda.api.entities.{VoiceChannel => JDAVoiceChannel}

import scala.jdk.CollectionConverters._

class VoiceChannel(channel: JDAVoiceChannel) {
  def members: List[Member] = channel.getMembers.asScala.toList.map(new Member(_))

  def toggleMuteAll: List[IO[Unit]] = members.map(_.toggleMute)
  def muteAll: List[IO[Unit]] = members.map(_.mute)
  def unmuteAll: List[IO[Unit]] = members.map(_.unmute)

  def denyPermission(role: Role, permission: Permission): IO[Unit] =
    channel.createPermissionOverride(role.role).setDeny(permission).toIO.as(())

  def allowPermission(role: Role, permission: Permission): IO[Unit] =
    channel.createPermissionOverride(role.role).setAllow(permission).toIO.as(())
}
