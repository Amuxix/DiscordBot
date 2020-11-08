package me.amuxix

import cats.effect.IO
import me.amuxix.wrappers.MessageEvent
import net.dv8tion.jda.api.events.message.MessageReceivedEvent
import net.dv8tion.jda.api.events.message.guild.GuildMessageReceivedEvent
import net.dv8tion.jda.api.events.message.priv.PrivateMessageReceivedEvent
import net.dv8tion.jda.api.hooks.ListenerAdapter

import scala.util.matching.Regex

class MessageListener(commands: Commands) extends ListenerAdapter { listener =>

  private def runCommands(commands: Map[Regex, (Regex, MessageEvent) => IO[Boolean]], event: MessageEvent): Unit =
    (for {
      recursive <- listener.commands.recursiveCommands.get
      _ <- if (!event.fromBot || (recursive && event.fromBot)) {
        commands.foldLeft(IO.pure(false)) {
          case (io, (regex, command)) if regex.matches(event.content) =>
            for {
              stopped <- io
              stop <- if (stopped) IO.pure(stopped) else command(regex, event)
            } yield stop
          case (io, _) => io
        }
      } else {
        IO.unit
      }
    } yield ()).unsafeRunSync()


  override def onMessageReceived(event: MessageReceivedEvent): Unit =
    runCommands(commands.onMessageReceived, MessageEvent.fromMessageReceivedEvent(event))

  override def onGuildMessageReceived(event: GuildMessageReceivedEvent): Unit =
    runCommands(commands.onGuildMessageReceived, MessageEvent.fromGuildMessageReceivedEvent(event))

  override def onPrivateMessageReceived(event: PrivateMessageReceivedEvent): Unit = {
    println(s"Received private message ${event.getAuthor.getName} - ${event.getMessage.getContentRaw}")
    runCommands(commands.onPrivateMessageReceived, MessageEvent.fromPrivateMessageReceivedEvent(event))
  }
}
