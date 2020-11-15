package me.amuxix.wrappers

import me.amuxix.Action
import me.amuxix.syntax.action._
import net.dv8tion.jda.api.entities.{Member => JDAMember}

import scala.jdk.CollectionConverters._

class Member(member: JDAMember) {
  lazy val id: Long = member.getIdLong

  lazy val asUser = new User(member.getUser)

  def isSelfMuted: Boolean = member.getVoiceState.isSelfMuted

  def isGuildMuted: Boolean = member.getVoiceState.isGuildMuted

  def mute: Action[Unit] = member.mute(true).as(())

  def unmute: Action[Unit] = member.mute(false).as(())

  def toggleMute: Action[Unit] = member.mute(!isGuildMuted).as(())

  def roles: Set[Role] = member.getRoles.asScala.toSet.map(new Role(_))

  def hasRole(role: Role): Boolean = roles.exists(_.id == role.id)

  def isGuildOwner: Boolean = member.getGuild.getOwnerIdLong == id
}
