package tech.igorramazanov.eventsbot

import java.time.format.DateTimeFormatter
import java.time.{Instant, LocalDateTime, ZoneId, ZonedDateTime}

import canoe.models.User
import cats.effect.Clock
import cats.syntax.functor._
import cats.{Functor, MonadError, Show}

import scala.concurrent.duration.SECONDS

object Utils {

  type MonadThrowable[F[_]] = MonadError[F, Throwable]
  def MonadThrowable[F[_]](implicit ev: MonadError[F, Throwable]) = ev

  implicit val showForZonedDateTime: Show[ZonedDateTime] =
    Show.show(zdt => zdt.format(DateTimeFormatter.ofPattern("HH:mm")))

  implicit class UserOps(val user: User) extends AnyVal {
    def fullName: String = user.firstName + user.lastName.fold("")(" " + _)
  }

  implicit class ClockOps[F[_]: Functor](clock: Clock[F]) {
    def now(zoneId: ZoneId): F[ZonedDateTime] =
      clock.realTime(SECONDS).map { epoch =>
        LocalDateTime
          .ofEpochSecond(epoch, 0, zoneId.getRules.getOffset(Instant.now()))
          .atZone(zoneId)
      }
  }

  implicit class AnyOps[A](private val v: A) extends AnyVal {
    @specialized def discard(): Unit = {
      val _: Any = v
      ()
    }
  }
}
