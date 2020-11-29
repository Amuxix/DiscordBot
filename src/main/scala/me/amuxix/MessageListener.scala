package me.amuxix

import cats.effect.IO
import me.amuxix.commands.{Command, ReactionCommand, StopSpam, TextCommand}
import me.amuxix.secrethitler.commands.{Discard, PickFascist, PickLiberal}
import me.amuxix.wrappers.MessageEvent._
import me.amuxix.wrappers.ReactionEvent._
import me.amuxix.wrappers.{Event, MessageEvent, ReactionEvent}
import net.dv8tion.jda.api.events.message.guild.GuildMessageReceivedEvent
import net.dv8tion.jda.api.events.message.guild.react.GuildMessageReactionAddEvent
import net.dv8tion.jda.api.events.message.priv.PrivateMessageReceivedEvent
import net.dv8tion.jda.api.events.message.priv.react.PrivateMessageReactionAddEvent
import net.dv8tion.jda.api.hooks.ListenerAdapter

object MessageListener extends ListenerAdapter { out =>

  private def runCommandList[T, E <: Event](
    event: E,
    commands: List[Command[T, E]],
    privateChannel: Boolean,
  )(
    log: (E, Command[T, E]) => IO[Unit],
  ): IO[Unit] =
    if (!event.author.isBot) {
      commands
        .sortBy(-_.pattern.toString.length)
        .foldLeft(IO.pure(false)) {
          case (io, command) if command.matches(event) =>
            for {
              stopped <- io
              stop <-
                if (stopped) {
                  IO.pure(true)
                } else {
                  log(event, command) *> command.run(command.pattern, event, privateChannel)
                }
            } yield stop
          case (io, _) => io
        }
        .as(())
    } else {
      IO.unit
    }

  private def runTextCommandList(
    event: MessageEvent,
    commands: List[TextCommand],
    privateChannel: Boolean,
  ): IO[Unit] =
    runCommandList(event, commands, privateChannel) { case (event, command) =>
      lazy val subgroups = command.pattern.findFirstMatchIn(event.content).get.subgroups.mkString(" ")
      if (command.pattern != Command.all) {
        IO(println(s"${event.author} issued text command $command $subgroups".trim))
      } else {
        IO.unit
      }
    }

  def runReactionCommandList(event: ReactionEvent, commands: List[ReactionCommand], privateChannel: Boolean): IO[Unit] =
    runCommandList(event, commands, privateChannel) { case (event, command) =>
      IO(println(s"${event.author} issued reaction command $command".trim))
    }

  override def onGuildMessageReceived(event: GuildMessageReceivedEvent): Unit =
    (for {
      commands <- Bot.enabledCommands(event.channel)
      textCommands = commands.collect { case command: TextCommand =>
        command
      }
      _ <- runTextCommandList(event, textCommands, privateChannel = false)
    } yield ()).unsafeRunSync()

  override def onPrivateMessageReceived(event: PrivateMessageReceivedEvent): Unit =
    runTextCommandList(event, List(Discard, StopSpam), privateChannel = true).unsafeRunSync()

  override def onGuildMessageReactionAdd(event: GuildMessageReactionAddEvent): Unit =
    (for {
      commands <- Bot.enabledCommands(event.channel)
      reactionCommands = commands.collect { case command: ReactionCommand =>
        command
      }
      _ <- runReactionCommandList(event, reactionCommands, privateChannel = false)
    } yield ()).unsafeRunSync()

  override def onPrivateMessageReactionAdd(event: PrivateMessageReactionAddEvent): Unit =
    runReactionCommandList(event, List(PickFascist, PickLiberal), privateChannel = true).unsafeRunSync()
}
