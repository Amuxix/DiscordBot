package me.amuxix.secrethitler

import cats.effect.IO
import me.amuxix.wrappers.{Message, User}

class President(
  president: User,
  overriden: Option[User] = None,
) {
  lazy val current: User = overriden.getOrElse(president)
  lazy val name: String = current.name
  lazy val mention: String = current.mention
  def sendMessage(message: String): IO[Message] = current.sendMessage(message)

  def withOverride(newPresident: User) = new President(president, Some(newPresident))

  def next(playerList: List[User]): President =
    new President(playerList((playerList.indexOf(president) + 1) % playerList.size))
}
