package me.amuxix

import java.io._

import cats.effect.concurrent.Ref
import cats.effect.{IO, Resource}
import cats.instances.list._
import cats.syntax.foldable._
import me.amuxix.Bot.{allCommands, allowedRoles, enabledCommands}

import scala.jdk.CollectionConverters._

object Persistence {
  val fileFolder = new File("data")
  val replacementsFile = new File(fileFolder, "replacements.txt")
  val allowedRolesFile = new File(fileFolder, "allowedRoles.txt")
  val enabledCommandsFile = new File(fileFolder, "enabledCommands.txt")

  private def save[T](file: File, ref: Ref[IO, T])(f: T => List[String]): IO[Unit] =
    for {
      toSave <- ref.get
      _ <- Resource.fromAutoCloseable(IO(new PrintWriter(new FileWriter(file)))).use { writer =>
        f(toSave).traverse_(l => IO(writer.print(s"$l\n")))
      }
    } yield ()

  private def load[T](file: File, ref: Ref[IO, T])(f: List[String] => T): IO[Unit] =
    if (file.exists()) {
      for {
        loaded <- Resource.fromAutoCloseable(IO(new BufferedReader(new FileReader(file)))).use { reader =>
          IO(reader.lines().iterator().asScala.toList).map(f)
        }
        _ <- ref.set(loaded)
      } yield ()
    } else {
      IO.unit
    }

  val saveAllowedRoles: IO[Unit] = save(allowedRolesFile, allowedRoles)(_.toList.map(_.toString))

  val loadAllowedRoles: IO[Unit] = load(allowedRolesFile, allowedRoles)(_.map(_.toLong).toSet)

  val saveEnabledCommands: IO[Unit] = save(enabledCommandsFile, enabledCommands)(_.toList.map {
    case (channelID, commands) => s"$channelID, ${commands.mkString(", ")}"
  })

  val loadEnabledCommands: IO[Unit] = load(enabledCommandsFile, enabledCommands)(_.flatMap(_.split(", ").toList match {
    case Nil            => None
    case channel :: Nil => Some(channel.toLong -> Set.empty[AnyCommand])
    case channel :: tail =>
      val commands = tail.flatMap(commandName => allCommands.find(_.className.equalsIgnoreCase(commandName))).toSet
      Some(channel.toLong -> commands)
  }).toMap)
}
