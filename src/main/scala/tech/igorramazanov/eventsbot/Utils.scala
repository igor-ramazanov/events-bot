package tech.igorramazanov.eventsbot

import java.time.{Instant, LocalDateTime, ZoneId, ZonedDateTime}

import canoe.api.models.ChatApi
import canoe.models.PrivateChat
import cats.effect.{Blocker, Clock, ContextShift}
import cats.syntax.functor._
import cats.{Functor, MonadError}

import scala.concurrent.duration.SECONDS

object Utils {

  type MonadThrowable[F[_]] = MonadError[F, Throwable]
  def MonadThrowable[F[_]](implicit
      ev: MonadError[F, Throwable]
  ): MonadError[F, Throwable] = ev

  implicit class ClockOps[F[_]: Functor](clock: Clock[F]) {
    def now(zoneId: ZoneId): F[ZonedDateTime] =
      clock.realTime(SECONDS).map { epoch =>
        LocalDateTime
          .ofEpochSecond(epoch, 0, zoneId.getRules.getOffset(Instant.now()))
          .atZone(zoneId)
      }
  }

  implicit def user2chatApi(u: model.User): ChatApi =
    new ChatApi(
      PrivateChat(u.id.toLong, Option(u.username), Option(u.firstName), None)
    )

  def ioOp[F[_]: ContextShift, A](
      fa: F[A]
  )(implicit blocker: Blocker): F[A] =
    ContextShift[F].blockOn(blocker)(fa)
}
