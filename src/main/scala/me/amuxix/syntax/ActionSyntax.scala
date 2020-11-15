package me.amuxix.syntax

import me.amuxix.Action
import net.dv8tion.jda.api.requests.RestAction

trait ActionSyntax {
  @inline implicit final def actionOps[A](action: RestAction[A]): Action[A] = new Action(action)
}
