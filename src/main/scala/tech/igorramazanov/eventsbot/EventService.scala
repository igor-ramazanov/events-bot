package tech.igorramazanov.eventsbot

import java.time.ZoneId

import canoe.api._
import canoe.syntax._
import canoe.models.outgoing.LocationContent
import cats._
import cats.effect._
import cats.effect.concurrent._
import cats.effect.implicits._
import cats.implicits._
import simulacrum.typeclass
import tech.igorramazanov.eventsbot.Utils._
import tech.igorramazanov.eventsbot.i18n.Language
import tech.igorramazanov.eventsbot.model._
import tech.igorramazanov.eventsbot.storage._

import scala.concurrent.duration._
import scala.util.chaining._

@typeclass trait EventService[F[_]] {
  def save(user: User): F[Unit]
  def show(user: User): F[Unit]
  def join(user: User): F[Unit]
  def leave(user: User): F[Unit]
  def delete(user: User): F[Unit]
  def create(
      user: User,
      maybeDescription: Option[String],
      hh: Int,
      mm: Int,
      longitude: Double,
      latitude: Double
  ): F[Unit]
  def post(news: String): F[Unit]
}

object EventService {

  val remindingPeriod: FiniteDuration = 5.minutes

  def delays[F[_]: Timer: Monad](
      state: State,
      zoneId: ZoneId
  ): F[(FiniteDuration, FiniteDuration, Boolean)] =
    for {
      now <- Timer[F].clock.now(zoneId).map(_.toEpochSecond)
      notificationDelay = (state.when.toEpochSecond - now).seconds
      remindingDelay = notificationDelay - remindingPeriod
    } yield (
      remindingDelay,
      notificationDelay,
      remindingDelay >= Duration.Zero
    )

  def schedule[F[_]: Concurrent: Timer: Storage: TelegramClient](
      fibers: Ref[F, List[Fiber[F, Unit]]],
      zoneId: ZoneId,
      language: Language
  ): F[String] =
    for {
      state <- Storage[F].state
      (remindingDelay, notificationDelay, withReminder) <- delays(state, zoneId)
      remind = for {
        _ <- Timer[F].sleep(remindingDelay)
        ps <- Storage[F].participants
        _ <- ps.traverse(_.send(language.reminder))
      } yield ()
      notify = for {
        _ <- Timer[F].sleep(notificationDelay)
        pastState = state.copy(past = true)
        _ <- Storage[F].save(pastState)
        ps <- Storage[F].participants
        _ <- ps.traverse(u =>
          u.send(language.start(pastState, ps)) >>
            u.send(LocationContent(pastState.latitude, pastState.longitude))
        )
      } yield ()
      _ <- (for {
          remindingFiber <- remind.whenA(withReminder).start
          notificationFiber <- notify.start
          prevFibers <-
            fibers.getAndSet(List(remindingFiber, notificationFiber))
          _ <- prevFibers.traverse(_.cancel.attempt)
        } yield ()).whenA(!state.past)
    } yield
      (if (withReminder) language.schedulingConfirmationFull
       else language.schedulingConfirmationShort) + "\n"

  def create[F[_]: Concurrent: Timer: Storage: TelegramClient](
      state: State,
      zoneId: ZoneId,
      language: Language
  ): F[EventService[F]] =
    for {
      now <- Timer[F].clock.now(zoneId)
      expired = now.isAfter(state.when)
      _ <- Storage[F].save(state.copy(past = true)).whenA(expired)
      fibers <- Ref.of[F, List[Fiber[F, Unit]]](Nil)
      _ <- schedule(fibers, zoneId, language)
      respond =
        (u: User, s: String) =>
          for {
            ps <- Storage[F].participants
            state <- Storage[F].state
            _ <-
              if (state.past) u.send(language.state(state, ps)).void
              else
                u.send(s + language.state(state, ps)) >>
                  u.send(LocationContent(state.latitude, state.longitude)).void
          } yield ()
      notifyPeers =
        (buildMessage: (String, List[User]) => String, originator: User) => {
          for {
            users <- Storage[F].users
            ps <- Storage[F].participants
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

      def save(user: User): F[Unit] = Storage[F].join(user.id)

      def show(user: User): F[Unit] =
        respond(user, "")

      def join(user: User): F[Unit] =
        for {
          state <- Storage[F].state
          _ <- Storage[F].join(user.id).whenA(!state.past)
          ps <- Storage[F].participants
          (_, _, withReminder) <- delays(state, zoneId)
          _ <-
            if (state.past)
              respond(user, language.state(state, ps))
            else
              respond(
                user,
                if (withReminder) language.schedulingConfirmationFull
                else language.schedulingConfirmationShort
              ) >> notifyPeers(language.notifyJoining, user)
        } yield ()

      def leave(user: User): F[Unit] =
        for {
          ps <- Storage[F].participants
          state <- Storage[F].state
          exists = ps.map(_.id).toSet(user.id)
          _ <- Storage[F].leave(user.id)
          ps <- Storage[F].participants
          _ <-
            if (exists)
              if (state.past)
                respond(user, language.state(state, ps))
              else
                Storage[F].save(state) >>
                  respond(user, language.left) >>
                  notifyPeers(language.notifyLeaving, user)
            else
              respond(user, language.didNotExist)
        } yield ()

      def delete(user: User): F[Unit] = Storage[F].delete(user.id)

      def create(
          user: User,
          maybeDescription: Option[String],
          hh: Int,
          mm: Int,
          longitude: Double,
          latitude: Double
      ): F[Unit] = {
        val run = for {
          now <- Timer[F].clock.now(zoneId)
          date = now.withHour(hh).withMinute(mm).pipe { d =>
            // Potential midnight shift
            if (d.isBefore(now)) d.plusDays(1) else d
          }
          state = State(
            maybeDescription,
            longitude,
            latitude,
            date,
            past = false
          )
          _ <- Storage[F].save(state)
          users <- Storage[F].users
          _ <- users.traverse(u =>
            if (u.id === user.id) Storage[F].join(user.id)
            else Storage[F].leave(u.id)
          )
          response <- schedule(fibers, zoneId, language)
          _ <- for {
            users <- Storage[F].users
            state <- Storage[F].state
            _ <- users.traverse { peer =>
              if (peer.id != user.id) {
                val message = language.created(user.username, state)
                peer.send(message).void
              } else
                ().pure[F]
            }
          } yield ()
        } yield response
        run >>= (r => respond(user, r))
      }

      def post(news: String): F[Unit] =
        for {
          users <- Storage[F].users
          // TODO: Sometimes fails for unknown reasons
          _ <- users.traverse { u =>
            u.send(news)
              .attempt
              .map(_.fold(e => println(e.getMessage), _ => ()))
          }.void
        } yield ()
    }
}
