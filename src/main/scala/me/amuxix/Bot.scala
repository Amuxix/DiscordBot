package me.amuxix

import cats.data.NonEmptyList
import cats.effect.{ExitCode, IO, IOApp, Ref}
import cats.syntax.parallel.*
import cats.syntax.foldable.*
import fs2.Stream
import me.amuxix.Persistence.{loadAllowedRoles, loadEnabledCommands}
import me.amuxix.commands.*
import me.amuxix.syntax.all.*
import me.amuxix.wrappers.{Channel, Role, User}
import net.dv8tion.jda.api.{JDA, JDABuilder}

import scala.concurrent.duration.*

object Bot extends IOApp:
  lazy val config = Configuration.fromConfig()

  given userMap: Ref[IO, Map[Long, User]] = Ref.unsafe[IO, Map[Long, User]](Map.empty)
  given roleMap: Ref[IO, Map[Long, Role]] = Ref.unsafe[IO, Map[Long, Role]](Map.empty)
  val spamList: Ref[IO, Set[Long]] = Ref.unsafe[IO, Set[Long]](Set.empty)
  val allowedRoles: Ref[IO, Set[Long]] = Ref.unsafe[IO, Set[Long]](Set.empty)
  val muteLeader: Ref[IO, Option[(Long, Boolean)]] = Ref.unsafe[IO, Option[(Long, Boolean)]](Option.empty)
  val replacements: Ref[IO, Map[String, String]] = Ref.unsafe[IO, Map[String, String]](Map.empty)

  val allCommands: NonEmptyList[AnyCommand] = NonEmptyList.of(
    Help,
    FollowMute,
    TakeOver,
    Mute,
    //Spam,
    //StopSpam,
  )

  lazy val textCommands: List[TextCommand] = allCommands.collect {
    case command: TextCommand => command
  }
  lazy val reactionCommands: List[ReactionCommand] = allCommands.collect {
    case command: ReactionCommand => command
  }
  lazy val slashCommands: List[SlashCommand] = allCommands.collect {
    case command: SlashCommand => command
  }

  val alwaysEnabled: NonEmptyList[AnyCommand] =
    NonEmptyList.of(
      Help,
      StopSpam,
    )

  val enabledCommands = Ref.unsafe[IO, Map[Long, Set[AnyCommand]]](Map.empty)

  def enabledCommands(channel: Channel): IO[NonEmptyList[AnyCommand]] =
    enabledCommands.get.map(enabledCommands => alwaysEnabled ++ enabledCommands.getOrElse(channel.id, Set.empty).toList)

  private val jdaIO: IO[JDA] =
    val jda = JDABuilder.createDefault(config.token).addEventListeners(MessageListener)
    IO(jda.build().awaitReady())

  def spam(jda: JDA): Stream[IO, Unit] =
    for
      spamList <- Stream.repeatEval(spamList.get).metered(1050.millis)
      id <- Stream.emits(spamList.toList)
      user <- Stream.eval(jda.getUserByID(id))
      _ <- user.sendMessage("Spam").streamed
    yield ()

  def followMute(jda: JDA): Stream[IO, Unit] =
    for
      muteLeaderTuple <- Stream.repeatEval(muteLeader.get).metered(500.millis)
      (leaderID, wasSelfMuted) <- Stream.emits(muteLeaderTuple.toList)
      leader <- Stream.eval(jda.getUserByID(leaderID))
      voiceChannel <- Stream.emits(leader.voiceChannel.toList)
      isSelfMuted <- Stream.emits(leader.isSelfMuted.toList)
      _ <- Stream.eval {
        if isSelfMuted && !wasSelfMuted then //Leader muted
          for
            _ <- voiceChannel.members.parTraverse_(_.mute)
            //role <- jda.getRoleByID(777994095342911519L)
            //_ <- role.fold(IO.unit)(voiceChannel.denyPermission(_, Permission.VOICE_SPEAK))
            _ <- muteLeader.set(Some((leaderID, isSelfMuted)))
          yield ()
        else if !isSelfMuted && wasSelfMuted then //Leader unmuted
          for
            _ <- voiceChannel.members.parTraverse_(_.unmute)
            //role <- jda.getRoleByID(777994095342911519L)
            //_ <- role.fold(IO.unit)(voiceChannel.allowPermission(_, Permission.VOICE_SPEAK))
            _ <- muteLeader.set(Some((leaderID, isSelfMuted)))
          yield ()
        else
          IO.unit
      }
    yield ()

  def registerSlashCommands(jda: JDA): IO[Unit] =
    allCommands.collect {
      case command: SlashCommand =>
        val data = command.pattern.build.setDefaultEnabled(Bot.alwaysEnabled.toList.contains(command))
        IO.println(s"""Registering "${data.getName}", enabled? ${data.isDefaultEnabled}""") *> jda.upsertCommand(data).toIO
    }.sequence_


  override def run(args: List[String]): IO[ExitCode] =
    (for
      _ <- Stream.eval(loadAllowedRoles)
      _ <- Stream.eval(loadEnabledCommands)
      jda <- Stream.eval(jdaIO)
      _ <- Stream.eval(registerSlashCommands(jda))
      _ <- spam(jda).concurrently(followMute(jda))
    yield ()).compile.drain.as(ExitCode.Success)

//Add Bot to server
//https://discord.com/oauth2/authorize?client_id=774637952131006484&scope=bot+applications.commands