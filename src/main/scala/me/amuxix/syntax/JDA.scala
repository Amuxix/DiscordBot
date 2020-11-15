package me.amuxix.syntax

import cats.effect.IO
import cats.effect.concurrent.Ref
import me.amuxix.wrappers.{Channel, Role, User}
import me.amuxix.syntax.action._
import net.dv8tion.jda.api.JDA
import net.dv8tion.jda.api.entities.Activity

import scala.jdk.CollectionConverters._

trait JDASyntax {
  @inline implicit final def jdaOps(jda: JDA): JDAOps = new JDAOps(jda)
}

final class JDAOps(private val jda: JDA) extends AnyVal {

  def getUserByID(id: Long)(implicit userMap: Ref[IO, Map[Long, User]]): IO[Option[User]] =
    userMap.get.flatMap {
      _.get(id).fold {
        for {
          jdaUser <- jda.retrieveUserById(id).get
          user = jdaUser.map(new User(_))
          _ <- user.fold(IO.unit)(role => userMap.update(_ + (id -> role)))
        } yield user
      }(role => IO.pure(Some(role)))
    }

  def getRoleByID(id: Long)(implicit roleMap: Ref[IO, Map[Long, Role]]): IO[Option[Role]] =
    roleMap.get.flatMap {
      _.get(id).fold {
        for {
          jdaRole <- IO(Option(jda.getRoleById(id)))
          role = jdaRole.map(new Role(_))
          _ <- role.fold(IO.unit)(role => roleMap.update(_ + (id -> role)))
        } yield role
      }(role => IO.pure(Some(role)))
    }

  def getChannelByID(id: Long): IO[Option[Channel]] = IO(Option(jda.getTextChannelById(id)).map(new Channel(_)))

  def getChannelByName(name: String): IO[Option[Channel]] =
    IO(jda.getTextChannelsByName(name, true).asScala.headOption.map(new Channel(_)))

  def setPlaying(what: String): Unit = jda.getPresence.setActivity(Activity.playing(what))
  //def setStreaming(what: String): Unit = jda.getPresence.setActivity(Activity.streaming(what))
  def setWatching(what: String): Unit = jda.getPresence.setActivity(Activity.watching(what))
  def clearActivity(): Unit = jda.getPresence.setActivity(null)

  def channels: List[Channel] = jda.getTextChannels.asScala.toList.map(new Channel(_))
  def findChannel(name: String): Option[Channel] = channels.find(_.name.equalsIgnoreCase(name))
}
