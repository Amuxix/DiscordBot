package me.amuxix

import net.dv8tion.jda.api.{JDA, JDABuilder}
import cats.effect.{ExitCode, IO, IOApp}
import fs2.Stream


object Bot extends IOApp {
  lazy val config = Configuration.fromConfig()

  lazy val listener = new Listener

  lazy val jdaIO: IO[JDA] = {
    val jda = JDABuilder.createDefault(config.token).addEventListeners(listener)
    IO(jda.build().awaitReady())
  }

  override def run(args: List[String]): IO[ExitCode] = {

    (for {
      jda <- Stream.eval(jdaIO)
      _ <- listener.followMute(jda).concurrently(listener.spam(jda))
    } yield ()).compile.drain.as(ExitCode.Success)
  }
}
