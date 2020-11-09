package me.amuxix

import java.io._

import cats.data.NonEmptyList
import cats.effect.concurrent.Ref
import cats.effect.{ExitCode, IO, IOApp, Resource}
import cats.syntax.foldable._
import fs2.Stream
import me.amuxix.Implicits._
import me.amuxix.Persistence.{loadAllowedRoles, loadEnabledCommands}
import me.amuxix.commands._
import me.amuxix.wrappers.{Channel, Role, User}
import net.dv8tion.jda.api.{JDA, JDABuilder}

import scala.concurrent.duration._

object Bot extends IOApp {
  lazy val config = Configuration.fromConfig()
  lazy val cs = this.contextShift

  implicit val userMap = Ref.unsafe[IO, Map[Long, User]](Map.empty)
  implicit val roleMap = Ref.unsafe[IO, Map[Long, Role]](Map.empty)
  val spamList = Ref.unsafe[IO, Set[Long]](Set.empty)
  val allowedRoles = Ref.unsafe[IO, Set[Long]](Set.empty)
  val muteLeader = Ref.unsafe[IO, Option[(Long, Boolean)]](Option.empty)
  val replacements = Ref.unsafe[IO, Map[String, String]](Map.empty)

  val allCommands =
    NonEmptyList.of(
      Help,
      EnableCommand,
      DisableCommand,
      FollowMute,
      TakeOver,
      Mute,
      Spam,
      StopSpam,
      AllowGroup,
      CopyAllowedCommands,
    )
  val alwaysEnabled = NonEmptyList.of(Help, EnableCommand, DisableCommand, StopSpam)

  val enabledCommands = Ref.unsafe[IO, Map[Long, Set[Command]]](Map.empty)

  def enabledCommands(channel: Channel): IO[NonEmptyList[Command]] =
    enabledCommands.get.map(enabledCommands => alwaysEnabled ++ enabledCommands.getOrElse(channel.id, Set.empty).toList)

  private val jdaIO: IO[JDA] = {
    val jda = JDABuilder.createDefault(config.token).addEventListeners(MessageListener)
    IO(jda.build().awaitReady())
  }

  def spam(jda: JDA): Stream[IO, Unit] =
    for {
      spamList <- Stream.repeatEval(spamList.get).metered(1050.millis)
      id <- Stream.emits(spamList.toList)
      maybeUser <- Stream.eval(jda.getUserByID(id))
      user <- Stream.emits(maybeUser.toList)
      privateChannel <- Stream.eval(user.privateChannel)
      _ <- privateChannel.sendMessage("Spam").streamed
    } yield ()

  def followMute(jda: JDA): Stream[IO, Unit] =
    for {
      muteLeaderTuple <- Stream.repeatEval(muteLeader.get).metered(500.millis)
      (leaderID, wasSelfMuted) <- Stream.emits(muteLeaderTuple.toList)
      maybeLeader <- Stream.eval(jda.getUserByID(leaderID))
      leader <- Stream.emits(maybeLeader.toList)
      voiceChannel = leader.voiceChannel
      isSelfMuted <- Stream.emits(leader.isSelfMuted.toList)
      _ <- Stream.eval {
        if (isSelfMuted && !wasSelfMuted) { //Leader muted
          for {
            _ <- muteLeader.set(Some(leaderID, isSelfMuted))
            _ <- voiceChannel.traverse_(_.muteAll)
          } yield ()
        } else if (!isSelfMuted && wasSelfMuted) { //Leader unmuted
          for {
            _ <- muteLeader.set(Some(leaderID, isSelfMuted))
            _ <- voiceChannel.traverse_(_.unmuteAll)
          } yield ()
        } else {
          IO.unit
        }
      }
    } yield ()

  override def run(args: List[String]): IO[ExitCode] =
    (for {
      _ <- Stream.eval(loadAllowedRoles)
      _ <- Stream.eval(loadEnabledCommands)
      jda <- Stream.eval(jdaIO)
      _ <- spam(jda).concurrently(followMute(jda))
    } yield ()).compile.drain.as(ExitCode.Success)
}
