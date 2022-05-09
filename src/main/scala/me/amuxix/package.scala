package me

import me.amuxix.commands.Command
import me.amuxix.wrappers.Event

package object amuxix {
  type AnyCommand = Command[?, ? <: Event]
}
