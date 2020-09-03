package tech.igorramazanov.eventsbot.service

import java.time.ZoneId

import cats.effect._
import tech.igorramazanov.eventsbot.Utils.MonadThrowable
import tech.igorramazanov.eventsbot.model.User
import tech.igorramazanov.eventsbot.storage.Storage
import cats.implicits._
import tech.igorramazanov.eventsbot.ui.i18n.I18N
import tech.igorramazanov.eventsbot.Utils._

trait GardenService[F[_]] {
  def persistent(u: User): F[Unit]
  def oneTime(u: User): F[Unit]
  def checkIfGardenTime(zoneId: ZoneId, i18n: I18N): F[Unit]
}

object GardenService {
  def create[F[_]: Storage: MonadThrowable: Timer: ContextShift](implicit
      blocker: Blocker
  ): F[GardenService[F]] =
    new GardenService[F] {
      override def persistent(u: User): F[Unit] =
        for {
          _ <- ioOp(
            Storage[F].save(u.copy(status = User.Status.PersistentGardener))
          )
        } yield ()

      override def oneTime(u: User): F[Unit] =
        for {
          _ <- ioOp(
            Storage[F].save(u.copy(status = User.Status.OneTimeGardener))
          )
        } yield ()

      //TODO: The same polling logic as in EventService
      override def checkIfGardenTime(zoneId: ZoneId, i18n: I18N): F[Unit] =
        for {
          _ <- Timer[F].clock.now(zoneId)
        } yield ()
    }.pure[F]
}
