package me.amuxix

import cats.data.NonEmptyList
import cats.effect.IO
import me.amuxix.commands.Command
import me.amuxix.wrappers.MessageEvent
import me.amuxix.wrappers.MessageEvent._
import net.dv8tion.jda.api.events.message.MessageReceivedEvent
import net.dv8tion.jda.api.events.message.priv.PrivateMessageReceivedEvent
import net.dv8tion.jda.api.hooks.ListenerAdapter

object MessageListener extends ListenerAdapter { out =>

  private def runCommandList(event: MessageEvent, commands: NonEmptyList[Command]): IO[Unit] =
    if (!event.author.isBot) {
      commands
        .foldLeft(IO.pure(false)) {
          case (io, command) if command.regex.matches(event.content) =>
            for {
              stopped <- io
              stop <- if (stopped) {
                IO.pure(stopped)
              } else {
                val subgroups = command.regex.findFirstMatchIn(event.content).get.subgroups.mkString(" ")
                if (command.regex != Command.all) println(s"${event.author} issued command $command $subgroups")
                command.run(command.regex, event)
              }
            } yield stop
          case (io, _) => io
        }
        .as(())
    } else {
      IO.unit
    }

  private def runCommands(event: MessageEvent): Unit =
    (for {
      commands <- Bot.enabledCommands(event.channel)
      _ <- runCommandList(event, commands)
    } yield ()).unsafeRunSync()

  override def onMessageReceived(event: MessageReceivedEvent): Unit =
    runCommands(event)
  //println(s"Received message ${event.getAuthor.getName} - ${event.getMessage.getContentRaw}")

  //override def onGuildMessageReceived(event: GuildMessageReceivedEvent): Unit = runCommands(event)

  override def onPrivateMessageReceived(event: PrivateMessageReceivedEvent): Unit =
    runCommandList(event, Bot.alwaysEnabled)
  //println(s"Received private message ${event.getAuthor.getName} - ${event.getMessage.getContentRaw}")
}
