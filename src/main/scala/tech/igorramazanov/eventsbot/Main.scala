package tech.igorramazanov.eventsbot

import java.time.ZoneId
import java.util.concurrent._
import java.util.concurrent.atomic.AtomicInteger

import canoe.api._
import cats.effect._
import cats.effect.concurrent.MVar
import cats.implicits._
import cats.effect.implicits._
import org.flywaydb.core.Flyway
import tech.igorramazanov.eventsbot.Utils.ioOp
import tech.igorramazanov.eventsbot.ui.i18n.I18N
import tech.igorramazanov.eventsbot.service.{EventService, NewsService}
import tech.igorramazanov.eventsbot.storage.Storage

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

object Main extends IOApp {
  protected val debug: Boolean = sys.props.get("debug").contains("true")

  implicit override protected val contextShift: ContextShift[IO] =
    IO.contextShift(
      ExecutionContext.fromExecutor(
        if (debug)
          // Can't use a single threaded pool, because it will block on the network socket.
          // When a thread reads a message then it puts it into some internal queue.
          // Now there is a choice:
          // 1) It can process that message
          // 2) Block again on the socket
          // This is a non-deterministic choice and 2nd is chosen then the message won't have chanced to be processed
          // until receiving another message causing this shift one more time (still non-deterministic).
          Executors.newFixedThreadPool(
            1,
            threadFactory("cpu")
          )
        else
          Executors.newFixedThreadPool(
            Runtime.getRuntime.availableProcessors(),
            threadFactory("cpu")
          )
      )
    )

  implicit protected val blocker: Blocker =
    Blocker.liftExecutionContext(
      ExecutionContext.fromExecutor(
        if (debug)
          Executors.newSingleThreadExecutor(threadFactory("io"))
        else
          new ThreadPoolExecutor(
            0,
            Int.MaxValue,
            60,
            TimeUnit.SECONDS,
            new SynchronousQueue[Runnable](false),
            threadFactory("io")
          )
      )
    )

  implicit override protected val timer: Timer[IO] =
    IO.timer(
      blocker.blockingContext,
      Executors.newSingleThreadScheduledExecutor(threadFactory("scheduler"))
    )

  override def run(args: List[String]): IO[ExitCode] =
    program[IO](
      i18n = I18N.Russian,
      token = sys.env("EVENTS_BOT_TELEGRAM_TOKEN"),
      database = sys.env("EVENTS_BOT_DATABASE"),
      zoneId = ZoneId.of(sys.env("EVENTS_BOT_TIMEZONE"))
    )

  private def threadFactory(name: String): ThreadFactory = {
    val counter = new AtomicInteger(0)
    (r: Runnable) => {
      val t = new Thread(r)
      t.setName(name + "-" + counter.getAndIncrement())
      t.setDaemon(false)
      t.setUncaughtExceptionHandler((_: Thread, e: Throwable) =>
        Thread.getDefaultUncaughtExceptionHandler match {
          case null => e.printStackTrace()
          case h    => h.uncaughtException(Thread.currentThread(), e)
        }
      )
      t
    }
  }
  // `-Ddebug=true` when curious about how threading works

  private def program[F[_]: ContextShift: Timer: ConcurrentEffect](
      i18n: I18N,
      token: String,
      database: String,
      zoneId: ZoneId
  ): F[ExitCode] =
    TelegramClient[F](token, blocker.blockingContext).use {
      implicit telegramClient =>
        for {
          _ <- runRdmbsMigrations[F](database)
          implicit0(storage: Storage[F]) <- Storage(database, blocker)
          state <- ioOp(storage.event)
          implicit0(eventService: EventService[F]) <-
            EventService.create[F](state, zoneId, I18N.Russian)
          _ <- runPeriodicEventChecks[F](zoneId, i18n).start
          implicit0(newsService: NewsService[F]) = NewsService.create[F]
          channel <- MVar.empty[F, (Boolean, Int)]
          _ <-
            Bot
              .hook("https://igorramazanov.tech/events-bot")
              .use(
                _.follow(
                  TelegramMessagesHandler
                    .handlers(i18n, channel, zoneId): _*
                ).compile.drain
              )
        } yield ExitCode.Success
    }

  private def runPeriodicEventChecks[F[
      _
  ]: Timer: ConcurrentEffect: EventService](
      zoneId: ZoneId,
      i18n: I18N
  ): F[Unit] =
    for {
      _ <- Timer[F].sleep(1.second)
      _ <- EventService[F].checkIfEventTime(zoneId, i18n)
      _ <- runPeriodicEventChecks[F](zoneId, i18n)
    } yield ()

  private def runRdmbsMigrations[F[_]: Sync](database: String): F[Unit] =
    Sync[F].delay {
      Flyway
        .configure()
        .dataSource(s"jdbc:sqlite:$database", "", "")
        .locations(
          new org.flywaydb.core.api.Location("classpath:db/migration")
        )
        .load()
        .migrate()
    }.void

}
