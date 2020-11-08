package me.amuxix

import net.dv8tion.jda.api.{JDA, JDABuilder}
import cats.effect.{Blocker, ExitCode, IO, IOApp, Resource}
import fs2.Stream
import java.io._

import cats.effect.concurrent.Ref
import net.dv8tion.jda.api.hooks.ListenerAdapter
import org.http4s.client.blaze.BlazeClientBuilder

import scala.concurrent.ExecutionContext.global
import scala.jdk.CollectionConverters._

object Bot extends IOApp {
  lazy val config = Configuration.fromConfig()
  val replacementsFile = new File("replacements.txt")

  def saveReplacements(replacements: Ref[IO, Map[String, String]]): IO[Unit] =
    Resource.fromAutoCloseable(IO(new PrintWriter(new FileWriter(Bot.replacementsFile)))).use { writer =>
      replacements.get.map(_.toList.flatMap {
        case (k, v) => List(k, v)
      }.foreach(l => writer.print(s"$l\n")))
    }

  val loadReplacements: IO[Map[String, String]] = {
    if (replacementsFile.exists()) {
      Resource.fromAutoCloseable(IO(new BufferedReader(new FileReader(replacementsFile)))).use { reader =>
        IO.pure(reader.lines().iterator().asScala.toList.grouped(2).map {
          case List(k, v) => (k, v)
        }.toMap)
      }
    } else {
      IO.pure(Map.empty)
    }
  }

  private def jdaIO(listener: ListenerAdapter): IO[JDA] = {
    val jda = JDABuilder.createDefault(config.token).addEventListeners(listener)
    IO(jda.build().awaitReady())
  }

  override def run(args: List[String]): IO[ExitCode] = {

    (for {
      replacements <- Stream.eval(loadReplacements)
      client <- Stream.resource(BlazeClientBuilder[IO](global).resource)
      blocker <- Stream.resource(Blocker[IO])
      commands = new Commands(replacements, client, blocker)
      listener = new MessageListener(commands)
      jda <- Stream.eval(jdaIO(listener))
      _ <- commands.streams(jda)
    } yield ()).compile.drain.as(ExitCode.Success)
  }
}
