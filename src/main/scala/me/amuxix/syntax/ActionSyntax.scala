package me.amuxix.syntax

import cats.effect.IO
import me.amuxix.Bot
import net.dv8tion.jda.api.requests.RestAction

import scala.concurrent.Promise

trait ActionSyntax:
  extension [A](action: RestAction[A])
    def toIO: IO[A] = IO.fromFuture(IO {
      val p = Promise[A]()
      action.queue(p.success, p.failure)
      p.future
    })
