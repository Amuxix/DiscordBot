package me.amuxix

import cats.effect.IO
import me.amuxix.commands.{Command, ReactionCommand, SlashCommand, StopSpam, TextCommand}
import me.amuxix.wrappers.event.ReactionEvent.*
import me.amuxix.wrappers.event.ReactionEvent.given
import me.amuxix.wrappers.event.MessageEvent.*
import me.amuxix.wrappers.event.MessageEvent.given
import me.amuxix.wrappers.event.SlashCommandEvent.*
import me.amuxix.wrappers.event.SlashCommandEvent.given
import me.amuxix.wrappers.event.Event
import net.dv8tion.jda.api.events.message.MessageReceivedEvent
import net.dv8tion.jda.api.events.message.react.MessageReactionAddEvent
import net.dv8tion.jda.api.events.interaction.command.SlashCommandInteractionEvent
import net.dv8tion.jda.api.hooks.ListenerAdapter
import cats.effect.unsafe.implicits.global
import me.amuxix.wrappers.event.{Event, MessageEvent, ReactionEvent, SlashCommandEvent}

object MessageListener extends ListenerAdapter:
  out =>

  private def runCommandList[T, E <: Event](
    event: E,
    commands: List[Command[T, E]],
    privateChannel: Boolean,
  )(
    log: (E, Command[T, E]) => IO[Unit],
  ): IO[Unit] =
    if !event.author.isBot then
      commands
        .sortBy(-_.pattern.toString.length)
        .foldLeft(IO.pure(false)) {
          case (io, command) if command.matches(event) =>
            for
              stopped <- io
              stop <-
                if stopped then
                  IO.pure(true)
                else
                  log(event, command) *> command.run(command.pattern, event, privateChannel)
            yield stop
          case (io, _) => io
        }
        .as(())
    else
      IO.unit

  private def runTextCommandList(
    event: MessageEvent,
    commands: List[TextCommand],
    privateChannel: Boolean,
  ): IO[Unit] =
    runCommandList(event, commands, privateChannel) { (event, command) =>
      lazy val subgroups = command.pattern.findFirstMatchIn(event.content).get.subgroups.mkString(" ")
      if command.pattern != Command.all then
        IO.println(s"${event.author} issued text command $command $subgroups".trim)
      else
        IO.unit
    }

  private def runReactionCommandList(event: ReactionEvent, commands: List[ReactionCommand], privateChannel: Boolean): IO[Unit] =
    runCommandList(event, commands, privateChannel) { (event, command) =>
      IO.println(s"${event.author} issued reaction command $command".trim)
    }

  private def runSlashCommandList(
    event: SlashCommandEvent,
    commands: List[SlashCommand],
    privateChannel: Boolean,
  ): IO[Unit] =
    runCommandList(event, commands, privateChannel) { (event, command) =>
      IO.println(s"${event.author} issued slash command $command".trim)
    }

  override def onMessageReceived(event: MessageReceivedEvent): Unit =
    (for
      commands <- Bot.enabledCommands(event.channel)
      textCommands = commands.collect { case command: TextCommand =>
        command
      }
      _ <- runTextCommandList(event, textCommands, privateChannel = false)
    yield ()).unsafeRunSync()

  override def onMessageReactionAdd(event: MessageReactionAddEvent): Unit =
    (for
      commands <- Bot.enabledCommands(event.channel)
      reactionCommands = commands.collect { case command: ReactionCommand =>
        command
      }
      _ <- runReactionCommandList(event, reactionCommands, privateChannel = false)
    yield ()).unsafeRunSync()

  override def onSlashCommandInteraction(event: SlashCommandInteractionEvent): Unit =
    (for
      commands <- Bot.enabledCommands(event.channel)
      reactionCommands = commands.collect { case command: SlashCommand =>
        command
      }
      _ <- runSlashCommandList(event, reactionCommands, privateChannel = false)
    yield ()).unsafeRunSync()
