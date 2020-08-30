package tech.igorramazanov.eventsbot.storage

import cats.effect._
import cats.implicits._
import doobie._
import doobie.implicits._
import simulacrum.typeclass
import tech.igorramazanov.eventsbot.model.{State, User}

@typeclass trait Storage[F[_]] {
  def save(u: User): F[Unit]
  def user(id: Int): F[Option[User]]
  def users: F[List[User]]
  def participants: F[List[User]]
  def join(id: Int): F[Unit]
  def leave(id: Int): F[Unit]
  def admin: F[User]
  def delete(id: Int): F[Unit]
  def save(s: State): F[Unit]
  def state: F[State]
}

object Storage {
  def apply[F[_]: Async: ContextShift](
      database: String,
      blocker: Blocker
  ): F[Storage[F]] =
    Sync[F].delay {
      val xa =
        Transactor.fromDriverManager[F](
          "org.sqlite.JDBC",
          s"jdbc:sqlite:$database",
          "",
          "",
          blocker
        )
      new Storage[F] with DoobieCodecs {
        override def save(u: User): F[Unit] =
          sql"INSERT OR REPLACE INTO user VALUES (${u.id}, ${u.firstName}, ${u.username}, ${u.status.id}, ${u.isParticipant})".update.run
            .transact(xa)
            .void

        override def user(id: Int): F[Option[User]] =
          sql"SELECT * FROM user WHERE id=$id"
            .query[User]
            .option
            .transact(xa)

        override def users: F[List[User]] =
          sql"SELECT * FROM user".query[User].to[List].transact(xa)

        override def participants: F[List[User]] =
          sql"SELECT * FROM user WHERE participant=1"
            .query[User]
            .to[List]
            .transact(xa)

        override def join(id: Int): F[Unit] =
          sql"UPDATE user SET participant=1 WHERE id=$id".update.run
            .transact(xa)
            .void

        override def leave(id: Int): F[Unit] =
          sql"UPDATE user SET participant=0 WHERE id=$id".update.run
            .transact(xa)
            .void

        override def admin: F[User] =
          sql"SELECT * FROM user WHERE status=${User.Status.Admin.id}"
            .query[User]
            .unique
            .transact(xa)

        override def delete(id: Int): F[Unit] =
          sql"DELETE FROM user WHERE id=$id".update.run.transact(xa).void

        override def save(s: State): F[Unit] = {
          val tx = for {
            _ <- sql"DELETE FROM state".update.run
            _ <-
              sql"INSERT INTO state VALUES (${s.description}, ${s.longitude}, ${s.latitude}, ${s.when.toString}, ${s.past})".update.run
          } yield ()
          tx.transact(xa)
        }

        override def state: F[State] =
          sql"SELECT * FROM state".query[State].unique.transact(xa)
      }
    }
}
