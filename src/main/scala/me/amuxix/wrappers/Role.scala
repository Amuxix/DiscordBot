package me.amuxix.wrappers

import net.dv8tion.jda.api.entities.{Role => JDARole}

class Role(role: JDARole) {
  lazy val id = role.getIdLong
  lazy val name = role.getName
}
