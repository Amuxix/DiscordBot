package me.amuxix.syntax

import cats.effect.IO
import me.amuxix.Bot
import net.dv8tion.jda.api.requests.RestAction

import scala.concurrent.Promise

trait ActionSyntax {
  @inline implicit final def actionOps[A](action: RestAction[A]): ActionOps[A] = new ActionOps[A](action)
  //@inline implicit final def action2IO[A](action: RestAction[A]): IO[A] = action.toIO
}

final class ActionOps[A](val action: RestAction[A]) extends AnyVal {

  def toIO: IO[A] = IO.fromFuture(IO {
    val p = Promise[A]()
    action.queue(p.success, p.failure)
    p.future
  })(Bot.cs)
}
