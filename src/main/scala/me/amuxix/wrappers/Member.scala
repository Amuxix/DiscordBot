package me.amuxix.wrappers

import cats.effect.IO
import net.dv8tion.jda.api.entities.{Member => JDAMember}
import me.amuxix.Implicits._

class Member(member: JDAMember) {
  lazy val id: Long = member.getIdLong

  def isSelfMuted: Boolean = member.getVoiceState.isSelfMuted

  def isGuildMuted: Boolean = member.getVoiceState.isGuildMuted

  def mute: IO[Unit] = member.mute(true).run

  def unmute: IO[Unit] = member.mute(false).run

  def toggleMute: IO[Unit] = member.mute(!isGuildMuted).run
}
