package tech.igorramazanov.eventsbot

import java.time.ZoneId

import cats._
import cats.effect._
import cats.effect.concurrent._
import cats.effect.implicits._
import cats.implicits._
import simulacrum.typeclass
import tech.igorramazanov.eventsbot.Utils._
import tech.igorramazanov.eventsbot.Strings.showState

import scala.concurrent.duration._
import scala.util.chaining._

@typeclass trait EventService[F[_]] {
  def save(user: User[F]): F[Unit]
  def show(user: User[F]): F[Unit]
  def join(user: User[F]): F[Unit]
  def leave(user: User[F]): F[Unit]
  def delete(user: User[F]): F[Unit]
  def create(
      user: User[F],
      maybeDescription: Option[String],
      hh: Int,
      mm: Int
  ): F[Unit]
}

object EventService {

  val remindingPeriod: FiniteDuration = 5.minutes

  def delays[F[_]: Timer: Monad](
      state: State[F],
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

  def schedule[F[_]: Concurrent: Timer: Storage](
      ref: Ref[F, State[F]],
      fibers: Ref[F, List[Fiber[F, Unit]]],
      zoneId: ZoneId
  ): F[String] =
    for {
      state <- ref.get
      (remindingDelay, notificationDelay, withReminder) <- delays(state, zoneId)
      remind = for {
        _ <- Timer[F].sleep(remindingDelay)
        state <- ref.get
        _ <- state.users.traverse(
          _.callback(
            Strings.Reminder
          )
        )
      } yield ()
      notify = for {
        _ <- Timer[F].sleep(notificationDelay)
        state <- ref.modify(
          _.copy(past = true).pipe(s => (s, s))
        )
        _ <- Storage[F].save(state)
        _ <- state.users.traverse(
          _.callback(Strings.Start[F](state))
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
      if (withReminder) Strings.FullConfirmation else Strings.ShortConfirmation

  def create[F[_]: Concurrent: Timer: Storage](
      state: State[F],
      zoneId: ZoneId
  ): F[EventService[F]] =
    for {
      now <- Timer[F].clock.now(zoneId)
      expired = now.isAfter(state.when)
      _ <- Storage[F].save(state.copy(past = true)).whenA(expired)
      ref <-
        Ref.of[F, State[F]](if (expired) state.copy(past = true) else state)
      fibers <- Ref.of[F, List[Fiber[F, Unit]]](Nil)
      _ <- schedule(ref, fibers, zoneId)
      respond =
        (u: User[F], s: String) =>
          ref.get.map(state =>
            if (state.past) state.show else s + state.show
          ) >>= u.callback
      notifyPeers =
        (buildMessage: (String, State[F]) => String, originator: User[F]) =>
          ref.get >>= { state =>
            state.all.traverse { peer =>
              if (peer.id != originator.id) {
                val message = buildMessage(originator.fullName, state)
                peer.callback(message)
              } else
                ().pure[F]
            }.void
          }
    } yield new EventService[F] {

      def save(user: User[F]): F[Unit] =
        ref.modify(_.save(user).pipe(s => (s, s))) >>= Storage[F].save

      def show(user: User[F]): F[Unit] =
        respond(user, "")

      def join(user: User[F]): F[Unit] =
        for {
          state <- ref.modify { state =>
            val newState =
              if (state.past)
                state
              else
                state.add(user)
            (newState, newState)
          }
          _ <- Storage[F].save(state)
          (_, _, withReminder) <- delays(state, zoneId)
          _ <-
            if (state.past)
              respond(user, state.show)
            else
              respond(
                user,
                if (withReminder) Strings.FullConfirmation
                else Strings.ShortConfirmation
              ) >> notifyPeers(Strings.JoinedNotifyPeers, user)
        } yield ()

      def leave(user: User[F]): F[Unit] =
        for {
          (state, existed) <- ref.modify { state =>
            val exists = state.ids.toSet(user.id)
            val newState =
              if (exists)
                state.deleteParticipant(user)
              else
                state
            newState.pipe(s => (s, s -> exists))
          }
          _ <-
            if (existed)
              if (state.past)
                respond(user, state.show)
              else
                respond(user, Strings.NotExisted)
            else
              Storage[F].save(state) >> respond(
                user,
                Strings.Left
              ) >> notifyPeers(Strings.LeftNotifyPeers, user)
        } yield ()

      def delete(user: User[F]): F[Unit] =
        for {
          state <-
            ref.modify(_.deleteCompletely(user).pipe(_.pipe(s => (s, s))))
          _ <- Storage[F].save(state)
        } yield ()

      def create(
          user: User[F],
          maybeDescription: Option[String],
          hh: Int,
          mm: Int
      ): F[Unit] = {
        val run = for {
          now <- Timer[F].clock.now(zoneId)
          date = now.withHour(hh).withMinute(mm).pipe { d =>
            // Potential midnight shift
            if (d.isBefore(now)) d.plusDays(1) else d
          }
          state <- ref.modify(prev =>
            State(
              maybeDescription,
              Map(user.id -> user),
              prev.all,
              date,
              past = false
            ).pipe(s => (s, s))
          )
          _ <- Storage[F].save(state)
          response <- schedule(ref, fibers, zoneId)
          _ <- notifyPeers(Strings.Created, user)
        } yield response
        run >>= (r => respond(user, r))
      }
    }
}
