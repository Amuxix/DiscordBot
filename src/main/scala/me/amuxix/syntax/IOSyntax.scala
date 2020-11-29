package me.amuxix.syntax

import cats.effect.IO
import fs2.Stream

trait IOSyntax {
  @inline implicit final def ioOps[A](io: IO[A]): IOOps[A] = new IOOps(io)
}

final class IOOps[A](val io: IO[A]) extends AnyVal {
  def streamed: Stream[IO, A] = Stream.eval(io)
}
