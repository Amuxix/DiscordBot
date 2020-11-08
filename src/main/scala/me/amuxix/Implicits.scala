package me.amuxix

import cats.effect.IO
import cats.effect.concurrent.Ref
import net.dv8tion.jda.api.entities.{Member, Message, VoiceChannel, User => JDAUser}
import net.dv8tion.jda.api.requests.RestAction

import scala.jdk.CollectionConverters._
import cats.implicits._
import me.amuxix.wrappers.User
import net.dv8tion.jda.api.JDA

object Implicits {
  //TODO Delete
  implicit class UserOps(user: JDAUser) {
    def findVoiceChannel: Option[VoiceChannel] = user.getJDA.getVoiceChannels.asScala.toList.collectFirst {
      case channel if channel.getMembers.asScala.exists(_.getUser.getIdLong == user.getIdLong) => channel
    }

    def member: Option[Member] = findVoiceChannel.flatMap(_.getMembers
      .asScala
      .find(_.getUser.getIdLong == user.getIdLong),
    )

    def isSelfMuted: Option[Boolean] = member.map(_.getVoiceState.isSelfMuted)
  }

  implicit class VoiceChannelOps(channel: VoiceChannel) {
    def muteAll: IO[Unit] =
      channel.getMembers.asScala.toList.traverse_ { member =>
        val isMuted = member.getVoiceState.isGuildMuted
        member.mute(!isMuted).run
      }
  }

  //TODO Delete
  implicit class MessageOps(message: Message) {
    def command(commands: String*)(f: => IO[Unit])(implicit recursiveCommands: Ref[IO, Boolean]): IO[Boolean] = {
      val content = message.getContentRaw
      for {
        recursive <- recursiveCommands.get
        stop <- if (commands.exists(content.equalsIgnoreCase)) f.as(!recursive) else IO.pure(false)
      } yield stop
    }
  }

  implicit class IOOps(io: IO[Boolean]) {
    def orStop(stop: Boolean): IO[Boolean] = if (stop) IO.pure(stop) else io
  }

  implicit class ActionOps[T](action: RestAction[T]) {
    def run: IO[Unit] = IO(action.queue())
  }

  implicit class JDAOps(jda: JDA) {
    def getUser(id: Long)(implicit userMap: Ref[IO, Map[Long, User]]): IO[User] =
      userMap.get.flatMap { users =>
        users.get(id).fold {
          for {
            jdaUser <- IO(jda.getUserById(id))
            user = new User(jdaUser)
            _ <- userMap.update(_ + (id -> user))
          } yield user
        }(IO.pure)
      }
  }
}
