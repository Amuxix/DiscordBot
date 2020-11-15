package me.amuxix

import cats.data.NonEmptyList
import cats.effect.IO
import me.amuxix.commands.{Command, StopSpam}
import me.amuxix.wrappers.MessageEvent
import me.amuxix.wrappers.MessageEvent._
import net.dv8tion.jda.api.events.message.guild.GuildMessageReceivedEvent
import net.dv8tion.jda.api.events.message.priv.PrivateMessageReceivedEvent
import net.dv8tion.jda.api.hooks.ListenerAdapter

object MessageListener extends ListenerAdapter { out =>

  private def runCommandList(event: MessageEvent, commands: NonEmptyList[Command], privateChannel: Boolean): IO[Unit] =
    if (!event.author.isBot) {
      commands
        .sortBy(-_.regex.toString.length)
        .foldLeft(IO.pure(false)) {
          case (io, command) if command.regex.matches(event.content) =>
            for {
              stopped <- io
              stop <-
                if (stopped) {
                  IO.pure(stopped)
                } else {
                  lazy val subgroups = command.regex.findFirstMatchIn(event.content).get.subgroups.mkString(" ")
                  if (command.regex != Command.all) println(s"${event.author} issued command $command $subgroups".trim)
                  command.run(command.regex, event, privateChannel)
                }
            } yield stop
          case (io, _) =>
            io
        }
        .as(())
    } else {
      IO.unit
    }

  override def onGuildMessageReceived(event: GuildMessageReceivedEvent): Unit =
    (for {
      commands <- Bot.enabledCommands(event.channel)
      _ <- runCommandList(event, commands, privateChannel = false)
    } yield ()).unsafeRunSync()

  override def onPrivateMessageReceived(event: PrivateMessageReceivedEvent): Unit =
    runCommandList(event, NonEmptyList.one(StopSpam), privateChannel = true).unsafeRunSync()
}
