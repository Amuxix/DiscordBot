package me.amuxix.wrappers

import java.util.concurrent.{CancellationException, CompletableFuture, CompletionException}

import cats.effect.IO
import net.dv8tion.jda.api.entities.{MessageChannel, Guild as JDAGuild, Member as JDAMember, Message as JDAMessage, User as JDAUser}
import net.dv8tion.jda.api.events.message.guild.GuildMessageReceivedEvent
import net.dv8tion.jda.api.events.message.priv.PrivateMessageReceivedEvent

class MessageEvent(
  jdaMessage: JDAMessage,
  jdaChannel: MessageChannel,
  jdaAuthor: JDAUser,
  jdaMember: Option[JDAMember],
  jdaGuild: Option[JDAGuild],
) extends Event(jdaChannel, jdaAuthor, jdaMember, jdaGuild):
  lazy val message: Message = new Message(jdaMessage)
  lazy val content: String = message.content
  lazy val authorName: String = jdaMember.flatMap(m => Option(m.getNickname)).getOrElse(author.name)

object MessageEvent:

  implicit def fromPrivateMessageReceivedEvent(event: PrivateMessageReceivedEvent): MessageEvent =
    new MessageEvent(event.getMessage, event.getChannel, event.getAuthor, None, None)

  implicit def fromGuildMessageReceivedEvent(event: GuildMessageReceivedEvent): MessageEvent =
    new MessageEvent(
      event.getMessage,
      event.getChannel,
      event.getAuthor,
      Option(event.getMember),
      Option(event.getGuild),
    )

  /*implicit class IOOps(io: IO.type) {

    def fromJavaFuture[A](cf: IO[CompletableFuture[A]]): IO[A] =
      cf.flatMap { cf =>
        IO.cancelable { cb =>
          cf.handle[Unit] { (result: A, err: Throwable) =>
            err match {
              case null =>
                cb(Right(result))
              case _: CancellationException =>
                ()
              case ex: CompletionException if ex.getCause ne null =>
                cb(Left(ex.getCause))
              case ex =>
                cb(Left(ex))
            }
          }
          IO(cf.cancel(true)).void
        }
      }
  }*/
