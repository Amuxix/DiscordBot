package me.amuxix.wrappers

import cats.effect.IO
import net.dv8tion.jda.api.entities.{Member => JDAMember}
import me.amuxix.Implicits._

import scala.jdk.CollectionConverters._

class Member(member: JDAMember) {
  lazy val id: Long = member.getIdLong

  lazy val asUser = new User(member.getUser)

  def isSelfMuted: Boolean = member.getVoiceState.isSelfMuted

  def isGuildMuted: Boolean = member.getVoiceState.isGuildMuted

  def mute: IO[Unit] = member.mute(true).run

  def unmute: IO[Unit] = member.mute(false).run

  def toggleMute: IO[Unit] = member.mute(!isGuildMuted).run

  def roles: Set[Role] = member.getRoles.asScala.toSet.map(new Role(_))

  def hasRole(role: Role): Boolean = roles.exists(_.id == role.id)

  def isGuildOwner: Boolean = member.getGuild.getOwnerIdLong == id
}
