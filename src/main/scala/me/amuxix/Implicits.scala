package me.amuxix

import cats.effect.IO
import cats.effect.concurrent.Ref
import net.dv8tion.jda.api.requests.RestAction
import me.amuxix.wrappers.User
import net.dv8tion.jda.api.JDA
import net.dv8tion.jda.api.entities.Activity

object Implicits {
  implicit class ActionOps[T](action: RestAction[T]) {
    def run: IO[Unit] = IO(action.queue())
  }

  implicit class JDAOps(jda: JDA) {

    def getUser(id: Long)(implicit userMap: Ref[IO, Map[Long, User]]): IO[User] =
      userMap.get.flatMap { users =>
        users
          .get(id)
          .fold {
            for {
              jdaUser <- IO(jda.getUserById(id))
              user = new User(jdaUser)
              _ <- userMap.update(_ + (id -> user))
            } yield user
          }(IO.pure)
      }

    def setPlaying(what: String): Unit = jda.getPresence.setActivity(Activity.playing(what))
    //def setStreaming(what: String): Unit = jda.getPresence.setActivity(Activity.streaming(what))
    def setWatching(what: String): Unit = jda.getPresence.setActivity(Activity.watching(what))
    def clearActivity: Unit = jda.getPresence.setActivity(null)
  }
}
