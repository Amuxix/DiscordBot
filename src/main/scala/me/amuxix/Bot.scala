package me.amuxix

import cats.data.NonEmptyList
import cats.effect.concurrent.Ref
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.parallel._
import fs2.Stream
import me.amuxix.Persistence.{loadAllowedRoles, loadEnabledCommands}
import me.amuxix.commands._
import me.amuxix.secrethitler.{Game, State}
import me.amuxix.secrethitler.commands._
import me.amuxix.secrethitler.commands.policies._
import me.amuxix.syntax.all._
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
  val secretHitler = Ref.unsafe[IO, Option[Game]](None)

  val secretHitlerCommands: NonEmptyList[AnyCommand] =
    Vote.all concatNel
      NonEmptyList.of(
        Investigate,
        Kill,
        SelectNextPresident,
        SecretHitler,
        Enact,
        JoinGame,
        LeaveGame,
        NominateChancellor,
        StartSecretHitler,
        Veto,
        Discard,
        PickFascist,
        PickLiberal,
        State,
      )

  val allCommands: NonEmptyList[AnyCommand] =
    secretHitlerCommands concatNel
      NonEmptyList.of(
        Help,
        EnableCommand,
        DisableCommand,
        EnableAllCommands,
        DisableAllCommands,
        FollowMute,
        TakeOver,
        Mute,
        Spam,
        StopSpam,
        AllowGroup,
        CopyAllowedCommands,
        DisallowGroup,
      )

  val alwaysEnabled: NonEmptyList[TextCommand] =
    NonEmptyList.of(
      Help,
      EnableCommand,
      DisableCommand,
      EnableAllCommands,
      DisableAllCommands,
      StopSpam,
      ListEnabledCommands,
    )

  val enabledCommands = Ref.unsafe[IO, Map[Long, Set[AnyCommand]]](Map.empty)

  def enabledCommands(channel: Channel): IO[NonEmptyList[AnyCommand]] =
    enabledCommands.get.map(enabledCommands => alwaysEnabled ++ enabledCommands.getOrElse(channel.id, Set.empty).toList)

  private val jdaIO: IO[JDA] = {
    val jda = JDABuilder.createDefault(config.token).addEventListeners(MessageListener)
    IO(jda.build().awaitReady())
  }

  def spam(jda: JDA): Stream[IO, Unit] =
    for {
      spamList <- Stream.repeatEval(spamList.get).metered(1050.millis)
      id <- Stream.emits(spamList.toList)
      user <- Stream.eval(jda.getUserByID(id))
      _ <- user.sendMessage("Spam").streamed
    } yield ()

  def followMute(jda: JDA): Stream[IO, Unit] =
    for {
      muteLeaderTuple <- Stream.repeatEval(muteLeader.get).metered(500.millis)
      (leaderID, wasSelfMuted) <- Stream.emits(muteLeaderTuple.toList)
      leader <- Stream.eval(jda.getUserByID(leaderID))
      voiceChannel <- Stream.emits(leader.voiceChannel.toList)
      isSelfMuted <- Stream.emits(leader.isSelfMuted.toList)
      _ <- Stream.eval {
        if (isSelfMuted && !wasSelfMuted) { //Leader muted
          for {
            _ <- voiceChannel.members.parTraverse_(_.mute)
            //role <- jda.getRoleByID(777994095342911519L)
            //_ <- role.fold(IO.unit)(voiceChannel.denyPermission(_, Permission.VOICE_SPEAK))
            _ <- muteLeader.set(Some((leaderID, isSelfMuted)))
          } yield ()
        } else if (!isSelfMuted && wasSelfMuted) { //Leader unmuted
          for {
            _ <- voiceChannel.members.parTraverse_(_.unmute)
            //role <- jda.getRoleByID(777994095342911519L)
            //_ <- role.fold(IO.unit)(voiceChannel.allowPermission(_, Permission.VOICE_SPEAK))
            _ <- muteLeader.set(Some((leaderID, isSelfMuted)))
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
