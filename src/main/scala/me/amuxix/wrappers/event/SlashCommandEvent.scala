package me.amuxix.wrappers.event

import cats.effect.IO
import me.amuxix.commands.slash.SlashPatternHelper
import me.amuxix.wrappers.Message
import me.amuxix.wrappers.event.{Event, SlashCommandEvent}
import me.amuxix.syntax.action.*
import net.dv8tion.jda.api.entities.{MessageChannel, Guild as JDAGuild, Member as JDAMember, User as JDAUser}
import net.dv8tion.jda.api.events.interaction.command.SlashCommandInteractionEvent

class SlashCommandEvent(
  jdaChannel: MessageChannel,
  jdaAuthor: JDAUser,
  jdaMember: Option[JDAMember],
  jdaGuild: Option[JDAGuild],
  underlying: SlashCommandInteractionEvent,
) extends GenericTextEvent(jdaChannel, jdaAuthor, jdaMember, jdaGuild):
  override def reply(string: String): IO[Unit] = underlying.reply(string).setEphemeral(true).toIO.void
  inline def getOption[T](option: String): T = SlashPatternHelper.getOption[T](underlying, option)
  lazy val name: String = underlying.getName

object SlashCommandEvent:
  given Conversion[SlashCommandInteractionEvent, SlashCommandEvent] = event =>
    new SlashCommandEvent(
      event.getChannel,
      event.getUser,
      Option(event.getMember),
      Option(event.getGuild),
      event,
    )
