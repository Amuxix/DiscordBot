package me.amuxix

import cats.effect.IO
import fs2.Stream
import net.dv8tion.jda.api.requests.RestAction

import scala.concurrent.Promise

final class Action[A](private val action: RestAction[A]) {

  private lazy val promise: Promise[A] = {
    val p = Promise[A]()
    action.queue(p.success, p.failure)
    p
  }

  def as[B](f: => B): Action[B] = new Action(action.map(_ => f))

  def get: IO[Option[A]] = IO.fromFuture(IO(promise.future))(Bot.cs).attempt.map(_.toOption)

  def map[B](f: A => B): Action[B] = new Action(action.map(f(_)))

  def run: IO[Unit] = get.as(())

  def streamed: Stream[IO, A] = for {
    option <- Stream.eval(get)
    a <- Stream.emits(option.toList)
  } yield a
}
