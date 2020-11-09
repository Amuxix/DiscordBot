package me.amuxix.wrappers

import net.dv8tion.jda.api.entities.{Guild => JDAGuild}

import scala.jdk.CollectionConverters._

class Guild(guild: JDAGuild) {
  lazy val ownerID: Long = guild.getOwnerIdLong
  def isOwner(user: User): Boolean = user.id == ownerID
  def isOwner(member: Member): Boolean = member.id == ownerID
  def roles: List[Role] = guild.getRoles.asScala.toList.map(new Role(_))
}
