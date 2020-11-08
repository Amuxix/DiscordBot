package me.amuxix

import java.io.File

import cats.implicits._
import cats.effect.concurrent.Ref
import cats.effect.{Blocker, ContextShift, IO, Timer}
import fs2.Stream
import org.http4s.client.Client
import me.amuxix.Implicits._
import me.amuxix.wrappers.{MessageEvent, User}
import net.dv8tion.jda.api.JDA
import org.http4s.EntityDecoder

import scala.util.matching.Regex
import scala.util.Random.nextInt
import scala.concurrent.duration._

class Commands(
  initialReplacements: Map[String, String] = Map.empty,
  client: Client[IO],
  blocker: Blocker,
)(
  implicit timer: Timer[IO],
  cs: ContextShift[IO],
) {

  val onMessageReceived: Map[Regex, (Regex, MessageEvent) => IO[Boolean]] = Map(
    "^toggle recursive commands$".r -> toggleRecursiveCommands,
    "^stop replacing (.+)$".r -> removeReplacement,
    "^mute all$".r -> muteAll,
    "^followmute$".r -> followMute,
    "^take over$".r -> takeOver,
    "^replace (.+),, (.+)$".r -> addReplacement,
    "^clear all replacements$".r -> clearReplacements,
    "^.+\\?$".r -> sentient,
    "^-play .+$".r -> sentient2,
    "^send file (.+)$".r -> sendFile,
    "^get (.+)$".r -> get,
    "^mute <@!(\\d+)>$".r -> mute,
    ".*".r -> aioque,
    ".*".r -> replace,
  )

  val onGuildMessageReceived: Map[Regex, (Regex, MessageEvent) => IO[Boolean]] =
    Map(
      "([Ss])pam <@!(\\d+)>".r -> addToSpam,
    )

  val onPrivateMessageReceived: Map[Regex, (Regex, MessageEvent) => IO[Boolean]] = Map(
    ".*".r -> removeFromSpam,
  )

  val spamList: Ref[IO, Set[Long]] = Ref.unsafe[IO, Set[Long]](Set.empty)
  implicit val userMap: Ref[IO, Map[Long, User]] = Ref.unsafe[IO, Map[Long, User]](Map.empty)
  val muteLeader: Ref[IO, Option[(Long, Boolean)]] = Ref.unsafe[IO, Option[(Long, Boolean)]](Option.empty)
  val replacements: Ref[IO, Map[String, String]] = Ref.unsafe[IO, Map[String, String]](initialReplacements)
  val cersibon: Ref[IO, Boolean] = Ref.unsafe[IO, Boolean](false)
  implicit val recursiveCommands: Ref[IO, Boolean] = Ref.unsafe[IO, Boolean](false)

  def spam(jda: JDA): Stream[IO, Unit] =
    for {
      spamList <- Stream.repeatEval(spamList.get).metered(400.millis)
      id <- Stream.emits(spamList.toList)
      user <- Stream.eval(jda.getUser(id))
      _ <- Stream.eval(user.openPrivateChannel { channel =>
        channel.sendMessage("Spam").queue()
      })
    } yield ()

  def followMute(jda: JDA): Stream[IO, Unit] =
    for {
      muteLeaderTuple <- Stream.repeatEval(muteLeader.get).metered(500.millis)
      (leaderID, lastState) <- Stream.emits(muteLeaderTuple.toList)
      leader <- Stream.eval(jda.getUser(leaderID))
      voiceChannel = leader.voiceChannel
      isSelfMuted <- Stream.emits(leader.isSelfMuted.toList)
      _ <- Stream.eval {
        if (isSelfMuted != lastState) {
          for {
            _ <- muteLeader.set(Some(leaderID, isSelfMuted))
            _ <- voiceChannel.traverse_(_.toggleMuteAll)
          } yield ()
        } else {
          IO.unit
        }
      }
    } yield ()

  def streams(jda: JDA): Stream[IO, Unit] = spam(jda).concurrently(followMute(jda))

  private val saveReplacements: IO[Unit] = Bot.saveReplacements(replacements)

  private def applyReplacements(string: String, replacements: Map[String, String]) =
    replacements.toList.foldLeft(string) {
      case (string, (regex, replacement)) =>
        string.replaceAll(regex, replacement)
    }

  def followMute(regex: Regex, event: MessageEvent): IO[Boolean] =
    for {
      leader <- muteLeader.get
      update <- leader match {
        case Some((id, _)) if id == event.author.id =>
          event.jda.clearActivity
          event.sendMessage(s"Stopped following ${event.author.name}.").as(None)
        case None =>
          event.jda.setWatching(event.author.name)
          event
            .sendMessage(s"Now following ${event.author.name}")
            .as(event.author.isSelfMuted.map(event.author.id -> _))
        case some @ Some((id, _)) =>
          for {
            following <- event.jda.getUser(id)
            _ <- event.sendMessage(s"Already following ${following.name}.")
          } yield some
      }
      _ <- muteLeader.set(update)
      recursive <- recursiveCommands.get
    } yield !recursive

  def takeOver(regex: Regex, event: MessageEvent): IO[Boolean] =
    for {
      leader <- muteLeader.get
      update <- leader match {
        case Some((id, _)) if id != event.author.id =>
          event.jda.setWatching(event.author.name)
          event
            .sendMessage(s"${event.author.name} taken over mute following.")
            .as(event.author.isSelfMuted.map(event.author.id -> _))
        case o => IO.pure(o)
      }
      _ <- muteLeader.set(update)
      recursive <- recursiveCommands.get
    } yield !recursive

  def toggleRecursiveCommands(regex: Regex, event: MessageEvent): IO[Boolean] =
    for {
      recursive <- recursiveCommands.get
      message = if (!recursive) "To understand recursion you must understand recursion" else "Do you understand it now?"
      _ <- event.sendMessage(message)
      _ <- recursiveCommands.set(!recursive)
    } yield true

  def removeReplacement(regex: Regex, event: MessageEvent): IO[Boolean] =
    event.content match {
      case regex(replacement) =>
        for {
          _ <- replacements.update(_ - replacement)
          _ <- saveReplacements
          recursive <- recursiveCommands.get
        } yield !recursive
      case _ => IO.pure(false)
    }

  def muteAll(regex: Regex, event: MessageEvent): IO[Boolean] = {
    println("Muting all users")
    event.author.voiceChannel.traverse_(_.toggleMuteAll).as(true)
  }

  def addReplacement(regex: Regex, event: MessageEvent): IO[Boolean] =
    event.content match {
      case regex(regex, replacement) =>
        for {
          _ <- replacements.update(_ + (regex -> replacement))
          _ <- saveReplacements
          recursive <- recursiveCommands.get
        } yield !recursive
      case _ => IO.pure(false)
    }

  def clearReplacements(regex: Regex, event: MessageEvent): IO[Boolean] =
    for {
      _ <- replacements.set(Map.empty)
      _ <- saveReplacements
    } yield true

  def sentient(regex: Regex, event: MessageEvent): IO[Boolean] =
    if (nextInt(10) == 0) event.sendMessage("YES I AGREE FELLOW HUMAN!").as(false) else IO.pure(false)

  def sentient2(regex: Regex, event: MessageEvent): IO[Boolean] =
    event.sendMessage("AH YES, LOVELY HUMAN MUSIC FOR MY HUMAN EARS").as(false)

  def sendFile(regex: Regex, event: MessageEvent): IO[Boolean] =
    event.content match {
      case regex(file) =>
        for {
          _ <- event.sendFile(new File(file))
          recursive <- recursiveCommands.get
        } yield !recursive
      case _ => IO.pure(false)
    }

  def get(regex: Regex, event: MessageEvent): IO[Boolean] =
    event.content match {
      case regex(url) =>
        for {
          file <- client.expect[File](url)(EntityDecoder.binFile[IO](new File(url.split("/").last), blocker))
          _ <- IO(println(file))
          _ <- event.sendFile(file)
          recursive <- recursiveCommands.get
        } yield !recursive
      case _ => IO.pure(false)
    }

  /*def callCersibon(regex: Regex, event: MessageEvent): IO[Boolean] =
    for {
      called <- cersibon.get
      message = if (!called) "A wild cersibon appears!" else "Cersibon goes away!"
      _ <- event.sendMessage(message)
      _ <- cersibon.set(!called)
      recursive <- recursiveCommands.get
    } yield !recursive

  def cersibonize(regex: Regex, event: MessageEvent): IO[Boolean] = ???*/

  def aioque(regex: Regex, event: MessageEvent): IO[Boolean] = {
    val possibleResponses = List(
      "It is certain.",
      "It is decidedly so.",
      "Without a doubt.",
      "Yes - definitely.",
      "You may rely on it.",
      "As I see it, yes.",
      "Most likely.",
      "Outlook good.",
      "Yes.",
      "Signs point to yes.",
      "Reply hazy, try again.",
      "Ask again later.",
      "Better not tell you now.",
      "Cannot predict now.",
      "Concentrate and ask again.",
      "Don't count on it.",
      "My reply is no.",
      "My sources say no.",
      "Outlook not so good.",
      "Very doubtful.",
    )
    if (event.author.name.toLowerCase.contains("velo") && nextInt(100) == 0) {
      for {
        _ <- event.message.addReaction("U+1F923")
        _ <- event.sendMessage(possibleResponses(nextInt(possibleResponses.size)))
      } yield false
    } else {
      IO.pure(false)
    }
  }

  def replace(regex: Regex, event: MessageEvent): IO[Boolean] =
    for {
      replacements <- replacements.get
      replacedContent = applyReplacements(event.content, replacements)
      _ <- if (!replacedContent.equals(event.content)) {
        event.sendMessage(s"$replacedContent")
      } else {
        IO.unit
      }
    } yield false

  def addToSpam(regex: Regex, event: MessageEvent): IO[Boolean] =
    event.content match {
      case regex(id) =>
        for {
          user <- event.jda.getUser(id.toLong)
          _ = println(s"Spamming ${user.name}")
          recursive <- recursiveCommands.get
          stop <- if (!user.isBot) spamList.update(_ + user.id).as(!recursive)
          else IO.pure(false)
        } yield stop
      case _ => IO.pure(false)
    }

  def removeFromSpam(regex: Regex, event: MessageEvent): IO[Boolean] =
    for {
      list <- spamList.get
      author = event.author
      id = author.id
      recursive <- recursiveCommands.get
      stop <- if (list.contains(id)) {
        IO(println(s"Stopped spamming ${author.name}")) *> spamList.update(_ - id).as(!recursive)
      } else {
        IO.pure(false)
      }
    } yield stop

  def mute(regex: Regex, event: MessageEvent): IO[Boolean] =
    event.content match {
      case regex(id) =>
        for {
          user <- event.jda.getUser(id.toLong)
          _ = println(s"Muting ${user.name}")
          recursive <- recursiveCommands.get
          stop <- if (!user.isBot)
            user.member.traverse_(_.toggleMute).as(!recursive)
          else IO.pure(false)
        } yield stop
      case _ => IO.pure(false)
    }
}
