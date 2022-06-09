package me

import me.amuxix.commands.Command
import me.amuxix.wrappers.event.Event

package object amuxix:
  type AnyCommand = Command[?, ? <: Event]
