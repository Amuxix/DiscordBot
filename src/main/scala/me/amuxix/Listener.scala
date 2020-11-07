package me.amuxix

import cats.effect.concurrent.Ref
import cats.effect.{IO, Timer}
import cats.implicits._
import fs2.Stream
import net.dv8tion.jda.api.JDA
import net.dv8tion.jda.api.entities._
import net.dv8tion.jda.api.events.message.MessageReceivedEvent
import net.dv8tion.jda.api.events.message.guild.GuildMessageReceivedEvent
import net.dv8tion.jda.api.events.message.priv.PrivateMessageReceivedEvent
import net.dv8tion.jda.api.hooks.ListenerAdapter
import net.dv8tion.jda.api.requests.RestAction

import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.Random
import scala.util.Random.nextInt

class Listener(implicit timer: Timer[IO]) extends ListenerAdapter {
  val spamList: Ref[IO, Set[Long]] = Ref.unsafe[IO, Set[Long]](Set.empty)
  val userMap: Ref[IO, Map[Long, User]] = Ref.unsafe[IO, Map[Long, User]](Map.empty)
  val muteLeader: Ref[IO, Option[(Long, Boolean)]] = Ref.unsafe[IO, Option[(Long, Boolean)]](Option.empty)
  val replacements: Ref[IO, Map[String, String]] = Ref.unsafe[IO, Map[String, String]](Map.empty)
  val cersibon: Ref[IO, Boolean] = Ref.unsafe[IO, Boolean](false)

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

  def aioque(channel: MessageChannel, author: User, message: Message): IO[Boolean] = {
    if (author.getName.toLowerCase.contains("velo") && nextInt(100) == 0) {
      for {
        _ <- message.addReaction("U+1F923").run
        _ <- channel.sendMessage(possibleResponses(nextInt(possibleResponses.size))).run
      } yield false
    } else {
      IO.pure(false)
    }
  }

  def applyReplacements(string: String, replacements: Map[String, String]) =
    replacements.toList.foldLeft(string) {
      case (string, (regex, replacement)) => string.replaceAll(regex, replacement)
    }

  def replace(message: Message, channel: MessageChannel): IO[Boolean] = {
    val content = message.getContentRaw
    replacements.get.flatMap { replacements =>
      val replacedContent = applyReplacements(content, replacements)
      if (!replacedContent.equals(content)) {
        channel.sendMessage(s"*$replacedContent*").run.as(false)
      } else {
        IO.pure(false)
      }
    }
  }

  def addReplacement(message: Message): IO[Boolean] = {
    val replace = "^replace (.+),, (.+)$".r
    message.getContentRaw match {
      case replace(regex, replacement) => replacements.update(_ + (regex -> replacement)).as(true)
      case _                           => IO.pure(false)
    }
  }

  def removeReplacement(message: Message): IO[Boolean] = {
    val replace = "^stop replacing (.+)$".r
    message.getContentRaw match {
      case replace(regex) => replacements.update(_ - regex).as(true)
      case _              => IO.pure(false)
    }
  }

  def swapPairs(words: String, nextInt: Int => Int) = {
    def swapPair(word: String) =
      if (nextInt(2) == 0) {
        val pairs = word.grouped(2).toList
        val swap = nextInt(pairs.size)
        pairs.zipWithIndex.foldLeft("") {
          case (word, (pair, `swap`)) => word + pair.reverse
          case (word, (pair, _))      => word + pair
        }
      } else {
        word
      }

    words.split(" ").map(swapPair).mkString(" ")
  }

  def dropLetter(word: String, nextInt: Int => Int) = word.filterNot(_ => nextInt(5) == 0)

  val shorthands = Map(
    "O que" -> "q",
    "voce" -> "ce",
    "quando" -> "qd",
    "valeu" -> "flw",
    "Portugues" -> "p'rt'gêsh",
  )

  def callCersibon(message: Message, channel: MessageChannel): IO[Boolean] =
    message.command("cersibon") {
      cersibon.get.flatMap { is =>
        val message = if (!is)
          channel.sendMessage("A wild cersibon appears!").run
        else {
          channel.sendMessage("Cersibon goes away!").run
        }
        message *> cersibon.set(!is)
      }
    }

  def cersibonize(message: Message, channel: MessageChannel): IO[Boolean] =
    cersibon.get.flatMap { is =>
      if (is) {
        val content = message.getContentRaw
        val random = new Random(content.hashCode)

        def nextInt(int: Int) = random.nextInt(int)

        val cersibonized = dropLetter(swapPairs(applyReplacements(content, shorthands), nextInt), nextInt)
        channel.sendMessage(cersibonized).run.as(false)
      } else {
        IO.pure(false)
      }
    }


  def spam(jda: JDA): Stream[IO, Unit] =
    for {
      spamList <- Stream.repeatEval(spamList.get).metered(400.millis)
      id <- Stream.emits(spamList.toList)
      user <- Stream.eval(getUser(id, jda))
      _ <- Stream.eval(IO(
        user.openPrivateChannel().queue { channel =>
          channel.sendMessage("Spam").queue()
        }
      )
      )
    } yield ()

  def followMute(jda: JDA): Stream[IO, Unit] =
    for {
      muteLeaderTuple <- Stream.repeatEval(muteLeader.get).metered(500.millis)
      (leaderID, lastState) <- Stream.emits(muteLeaderTuple.toList)
      leader <- Stream.eval(getUser(leaderID, jda))
      voiceChannel = leader.findVoiceChannel
      isSelfMuted <- Stream.emits(leader.isSelfMuted.toList)
      _ <- Stream.eval(if (isSelfMuted != lastState) {
        for {
          _ <- muteLeader.set(Some(leaderID, isSelfMuted))
          _ <- voiceChannel.traverse_(_.muteAll)
        } yield ()
      } else {
        IO.unit
      },
      )
    } yield ()

  def addToSpam(message: Message): IO[Boolean] = {
    val spamRegex = "spam <@!(\\d+)>".r
    message.getContentRaw.toLowerCase match {
      case spamRegex(id) =>
        for {
          user <- getUser(id.toLong, message.getJDA)
          _ = println(s"Spamming ${user.getName}")
          stop <- if (!user.isBot) spamList.update(_ + user.getIdLong).as(true) else IO.pure(false)
        } yield stop
      case _             => IO.pure(false)
    }
  }

  private def getUser(id: Long, jda: JDA) =
    userMap.get.flatMap { users =>
      users.get(id).fold {
        for {
          user <- IO(jda.getUserById(id))
          _ <- userMap.update(_ + (id -> user))
        } yield user
      }(IO.pure)
    }

  def removeFromSpam(user: User): IO[Boolean] =
    for {
      list <- spamList.get
      id = user.getIdLong
      stop <- if (list.contains(id)) {
        IO(println(s"Stopped spamming ${user.getName}")) *> spamList.update(_ - id).as(true)
      } else {
        IO.pure(false)
      }
    } yield stop

  def muteAll(message: Message, author: User): IO[Boolean] =
    message.command("muteall") {
      author.findVoiceChannel.traverse_ { channel =>
        channel.muteAll
      }
    }

  def followMute(message: Message, author: User): IO[Boolean] =
    message.command("followmute") {
      muteLeader.update {
        case Some((id, _)) if id == author.getIdLong => None
        case None                                    => author.isSelfMuted.map(author.getIdLong -> _)
        case some                                    => some
      }
    }

  implicit class UserOps(user: User) {
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

  implicit class MessageOps(message: Message) {
    def command(commands: String*)(f: => IO[Unit]): IO[Boolean] = {
      val content = message.getContentRaw
      if (commands.exists(content.equalsIgnoreCase)) f.as(true) else IO.pure(false)
    }
  }

  implicit class IOOps(io: IO[Boolean]) {
    def orStop(stop: Boolean): IO[Boolean] = if (stop) IO.pure(stop) else io
  }

  implicit class ActionOps[T](action: RestAction[T]) {
    def run: IO[Unit] = IO(action.queue())
  }

  override def onMessageReceived(event: MessageReceivedEvent): Unit = {
    if (!event.getAuthor.isBot) {
      (for {
        muteAll <- muteAll(event.getMessage, event.getAuthor)
        followMute <- followMute(event.getMessage, event.getAuthor).orStop(muteAll)
        addReplacement <- addReplacement(event.getMessage).orStop(followMute)
        removeReplacement <- removeReplacement(event.getMessage).orStop(addReplacement)
        callCersibon <- callCersibon(event.getMessage, event.getChannel).orStop(removeReplacement)
        aioque <- aioque(event.getChannel, event.getAuthor, event.getMessage).orStop(callCersibon)
        replace <- replace(event.getMessage, event.getChannel).orStop(aioque)
        cersibonize <- cersibonize(event.getMessage, event.getChannel).orStop(replace)
      } yield ()).unsafeRunSync()
    }
  }

  override def onGuildMessageReceived(event: GuildMessageReceivedEvent): Unit = {
    if (!event.getAuthor.isBot) {
      addToSpam(event.getMessage).unsafeRunSync()
    }
  }

  override def onPrivateMessageReceived(event: PrivateMessageReceivedEvent): Unit =
    if (!event.getAuthor.isBot) {
      removeFromSpam(event.getAuthor).unsafeRunSync()
    }
}
