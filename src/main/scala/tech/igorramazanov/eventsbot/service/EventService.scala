package tech.igorramazanov.eventsbot.service

import java.time.ZoneId

import canoe.api._
import canoe.models.outgoing.LocationContent
import canoe.syntax._
import cats.effect._
import cats.implicits._
import simulacrum.typeclass
import tech.igorramazanov.eventsbot.Utils._
import tech.igorramazanov.eventsbot.ui.i18n.I18N
import tech.igorramazanov.eventsbot.model._
import tech.igorramazanov.eventsbot.storage._

import scala.concurrent.duration._
import scala.util.chaining._

@typeclass trait EventService[F[_]] {
  def show(user: User): F[Unit]
  def join(user: User, zoneId: ZoneId): F[Unit]
  def leave(user: User): F[Unit]
  def create(
      user: User,
      maybeDescription: Option[String],
      hh: Int,
      mm: Int,
      longitude: Double,
      latitude: Double
  ): F[Unit]
  def checkIfEventTime(zoneId: ZoneId, i18n: I18N): F[Unit]
}

object EventService {
  private val remindingPeriod: FiniteDuration = 5.minutes

  def create[F[
      _
  ]: MonadThrowable: Timer: Storage: TelegramClient: ContextShift](
      event: Event,
      zoneId: ZoneId,
      i18n: I18N
  )(implicit blocker: Blocker): F[EventService[F]] =
    for {
      now <- Timer[F].clock.now(zoneId)
      expired = now.isAfter(event.when)
      _ <- ioOp(Storage[F].save(event.copy(past = true)).whenA(expired))
      respond =
        (u: User, s: String) =>
          for {
            ps <- ioOp(Storage[F].participants)
            event <- ioOp(Storage[F].event)
            _ <-
              if (event.past) u.send(i18n.event(event, ps)).void
              else
                u.send(s + i18n.event(event, ps)) >>
                  u.send(LocationContent(event.latitude, event.longitude)).void
          } yield ()
      notifyPeers =
        (buildMessage: (String, List[User]) => String, originator: User) => {
          for {
            users <- ioOp(Storage[F].users)
            ps <- ioOp(Storage[F].participants)
            _ <- users.traverse { peer =>
              if (peer.id != originator.id) {
                val message = buildMessage(originator.username, ps)
                peer.send(message).void
              } else
                ().pure[F]
            }
          } yield ()
        }
    } yield new EventService[F] {

      override def show(user: User): F[Unit] =
        respond(user, "")

      override def join(user: User, zoneId: ZoneId): F[Unit] =
        for {
          event <- ioOp(Storage[F].event)
          _ <- ioOp(Storage[F].join(user.id).whenA(!event.past))
          ps <- ioOp(Storage[F].participants)
          now <- Timer[F].clock.now(zoneId).map(_.toEpochSecond)
          notificationDelay = (event.when.toEpochSecond - now).seconds
          needsReminding = notificationDelay - remindingPeriod >= Duration.Zero
          _ <-
            if (event.past)
              respond(user, i18n.event(event, ps))
            else
              respond(
                user,
                if (needsReminding) i18n.schedulingConfirmationFull
                else i18n.schedulingConfirmationShort
              ) >> notifyPeers(i18n.notifyJoining, user)
        } yield ()

      override def leave(user: User): F[Unit] =
        for {
          ps <- ioOp(Storage[F].participants)
          event <- ioOp(Storage[F].event)
          exists = ps.map(_.id).toSet(user.id)
          _ <- ioOp(Storage[F].leave(user.id))
          ps <- ioOp(Storage[F].participants)
          _ <-
            if (exists)
              if (event.past)
                respond(user, i18n.event(event, ps))
              else
                ioOp(Storage[F].save(event)) >>
                  respond(user, i18n.left) >>
                  notifyPeers(i18n.notifyLeaving, user)
            else
              respond(user, i18n.didNotExist)
        } yield ()

      override def create(
          originator: User,
          maybeDescription: Option[String],
          hh: Int,
          mm: Int,
          longitude: Double,
          latitude: Double
      ): F[Unit] = {
        val run = for {
          now <- Timer[F].clock.now(zoneId)
          zonedDateTime = now.withHour(hh).withMinute(mm).pipe { d =>
            // Potential midnight shift
            if (d.isBefore(now)) d.plusDays(1) else d
          }
          notificationDelay =
            (zonedDateTime.toEpochSecond - now.toEpochSecond).seconds
          needsReminding = notificationDelay - remindingPeriod >= Duration.Zero
          event = Event(
            maybeDescription,
            longitude,
            latitude,
            zonedDateTime,
            past = false,
            needsReminding = needsReminding
          )
          _ <- ioOp(Storage[F].save(event))
          users <- ioOp(Storage[F].users)
          _ <- users.traverse(u =>
            if (u.id === originator.id) ioOp(Storage[F].join(originator.id))
            else ioOp(Storage[F].leave(u.id))
          )
          confirmation =
            (if (needsReminding) i18n.schedulingConfirmationFull
             else i18n.schedulingConfirmationShort) + "\n"
          _ <- for {
            users <- ioOp(Storage[F].users)
            event <- ioOp(Storage[F].event)
            _ <- users.traverse { u =>
              u.send(i18n.created(originator.username, event))
                .whenA(u.id != originator.id)
            }
          } yield ()
        } yield confirmation
        run >>= (r => respond(originator, r))
      }

      override def checkIfEventTime(zoneId: ZoneId, i18n: I18N): F[Unit] =
        for {
          e <- ioOp(Storage[F].event)
          now <- Timer[F].clock.now(zoneId).map(_.toEpochSecond)
          remind =
            !e.past &&
              e.needsReminding &&
              now - remindingPeriod.toSeconds >= e.when.toEpochSecond
          notify = !e.past && now >= e.when.toEpochSecond
          _ <- (ioOp(Storage[F].participants).map(
              _.traverse(_.send(i18n.reminder))
            ) >> ioOp(Storage[F].save(e.copy(needsReminding = false))))
            .whenA(remind)
          _ <- (ioOp(Storage[F].participants)
              .map { ps =>
                ps.traverse(u =>
                  u.send(i18n.start(e, ps)) >>
                    u.send(LocationContent(e.latitude, e.longitude))
                )
              } >> ioOp(Storage[F].save(e.copy(past = true))))
            .whenA(notify)
        } yield ()

    }
}
