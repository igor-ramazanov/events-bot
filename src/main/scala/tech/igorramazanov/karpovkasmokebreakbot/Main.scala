package tech.igorramazanov.karpovkasmokebreakbot

import java.time.ZoneId
import java.util.concurrent._
import java.util.concurrent.atomic.AtomicInteger

import canoe.api._
import canoe.api.models.Keyboard
import canoe.models.messages.TextMessage
import canoe.models.{KeyboardButton, ReplyKeyboardMarkup}
import canoe.syntax._
import cats.effect._
import cats.effect.concurrent.MVar
import cats.implicits._
import org.http4s.client.JavaNetClientBuilder
import org.slf4j.LoggerFactory
import tech.igorramazanov.karpovkasmokebreakbot.Command.commandShow
import tech.igorramazanov.karpovkasmokebreakbot.Utils._

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.util.chaining._

object Main extends IOApp {
  def threadFactory(name: String) = {
    val counter = new AtomicInteger(0)
    new ThreadFactory {
      override def newThread(r: Runnable): Thread = {
        val t = new Thread(r)
        t.setName(name + "-" + counter.getAndIncrement())
        t.setDaemon(false)
        t.setUncaughtExceptionHandler(new Thread.UncaughtExceptionHandler {
          override def uncaughtException(t: Thread, e: Throwable): Unit =
            Thread.getDefaultUncaughtExceptionHandler match {
              case null => e.printStackTrace()
              case h    => h.uncaughtException(Thread.currentThread(), e)
            }
        })
        t
      }
    }
  }

  // `-Ddebug=true` when curious about how threading works
  val debug: Boolean = sys.props.get("debug").contains("true")

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
            2,
            threadFactory("cpu")
          )
        else
          Executors.newFixedThreadPool(
            math.min(Runtime.getRuntime.availableProcessors(), 4),
            threadFactory("cpu")
          )
      )
    )

  implicit val blocker: Blocker = Blocker.liftExecutionContext(
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

  val logger = LoggerFactory.getLogger(getClass)

  val token: String = sys.env("SMOKE_BREAK_BOT_TELEGRAM_TOKEN")
  val zoneId: ZoneId = ZoneId.of(sys.env("SMOKE_BREAK_BOT_TIMEZONE"))
  val dataDir: String = sys.env("SMOKE_BREAK_BOT_DATA_DIR")
  val usersFile: String = dataDir + "/" + "users.txt"
  val rejectedUsersFile: String = dataDir + "/" + "rejected_users.txt"
  val stateFile: String = dataDir + "/" + "state.txt"
  val pollingPeriod = 1.second

  def validCommands: Expect[TextMessage] =
    textMessage.matching(Command.values.map(_.show).mkString("|"))

  def run(args: List[String]): IO[ExitCode] = {
    implicit val telegramClient = TelegramClient
      .fromHttp4sClient[IO](token)(JavaNetClientBuilder[IO](blocker).create)
    for {
      _ <- IO(logger.info("Bot is starting"))
      implicit0(storage: Storage[IO]) <-
        Storage
          .create[IO](
            usersFile,
            stateFile,
            zoneId
          )
      state <- ioOp(storage.restoreState)
      implicit0(smokeService: SmokeService[IO]) <-
        SmokeService.create[IO](state, zoneId)
      channel <- MVar.empty[IO, (Boolean, Int)]
      fiber <-
        Bot
          .polling[IO]
          .follow(
            approvals(channel),
            smokeBreak(channel)
          )
          .compile
          .drain
          .start
      _ <- IO(logger.info("Bot started"))
      _ <- fiber.join
    } yield ExitCode.Success
  }

  def approvals[F[_]: TelegramClient: MonadThrowable: Storage](
      channel: MVar[F, (Boolean, Int)]
  ): Scenario[F, Unit] =
    for {
      adminId <- Scenario.eval(Storage[F].adminId)
      (isApproved, id) <- Scenario.expect(
        textMessage
          .matching("(\\/yes_[0-9]+)|(\\/no_[0-9]+)")
          .when(_.from.exists(_.id === adminId))
          .andThen(m =>
            m.text
              .split('_')
              .pipe(a =>
                (
                  a.head.tail match {
                    case "yes" => true
                    case "no"  => false
                  },
                  a.last.toInt
                )
              )
          )
      )
      _ <- Scenario.eval(channel.put((isApproved, id)))
    } yield ()

  def smokeBreak[F[
      _
  ]: TelegramClient: Timer: MonadThrowable: Storage: SmokeService: ContextShift](
      channel: MVar[F, (Boolean, Int)]
  )(implicit blocker: Blocker): Scenario[F, Unit] =
    for {
      m <- Scenario.expect(validCommands)
      _ <- m.from.fold(Scenario.done[F]) { u =>
        val sender =
          User[F](u.id, u.fullName, s => m.chat.send(s).attempt.void)
        for {
          status <- Scenario.eval(Storage[F].status(u))
          start =
            m.chat
              .send(
                Strings.Greeting,
                keyboard = Keyboard.Reply(
                  ReplyKeyboardMarkup(
                    List(
                      List(
                        KeyboardButton.text("/show"),
                        KeyboardButton.text("/leave"),
                        KeyboardButton.text("/delete")
                      ),
                      List(
                        KeyboardButton.text("/join"),
                        KeyboardButton.text("/create"),
                        KeyboardButton.text("/help")
                      )
                    ),
                    resizeKeyboard = true.some
                  )
                )
              )
              .attempt
              .void
          signUp = u.username.fold(
            Scenario.eval(m.chat.send(Strings.MustHavePublicUsername).void)
          ) { username =>
            status match {
              case UserStatus.New =>
                Scenario
                  .eval(
                    m.chat.send(
                      Strings.SignUp,
                      keyboard = Keyboard.Reply(
                        ReplyKeyboardMarkup.singleButton(
                          KeyboardButton.text(Command.Signup.show),
                          resizeKeyboard = true.some
                        )
                      )
                    ) >> Storage[F]
                      .save(m.chat, u, UserStatus.WaitingConfirmation)
                  )
              case UserStatus.WaitingConfirmation
                  if Command
                    .withName(m.text.tail.capitalize) === Command.Signup =>
                for {
                  adminChat <- Scenario.eval(ioOp(Storage[F].adminChat))
                  _ <- Scenario.eval(
                    adminChat.send(Strings.Request(username, u.id))
                  )
                  signedUp <- Scenario.eval(
                    (Timer[F].sleep(pollingPeriod) >> channel.read)
                      .iterateUntil(_._2 === u.id)
                      .map(_._1)
                  )
                  _ <- Scenario.eval(channel.take)
                  _ <-
                    if (signedUp)
                      Scenario.eval(
                        ioOp(
                          Storage[F].save(m.chat, u, UserStatus.SignedIn)
                        ) >> SmokeService[F]
                          .save(sender) >> m.chat
                          .send(Strings.SignedUp) >> start
                      )
                    else
                      Scenario.eval(
                        ioOp(
                          Storage[F].save(m.chat, u, UserStatus.Rejected)
                        ) >> m.chat.send(Strings.Rejected)
                      )
                } yield ()
              case _ => Scenario.done[F]
            }
          }

          program = Command.withName(m.text.tail.capitalize) match {
            case Command.Start | Command.Help =>
              Scenario.eval(start)
            case Command.Show =>
              Scenario.eval(SmokeService[F].show(sender))
            case Command.Leave =>
              Scenario.eval(SmokeService[F].leave(sender))
            case Command.Delete =>
              Scenario
                .eval(
                  SmokeService[F].delete(sender) >>
                    ioOp(Storage[F].delete(u)) >>
                    m.chat.send(Strings.Deleted).void
                )
            case Command.Join =>
              Scenario.eval(SmokeService[F].join(sender))
            case Command.Create =>
              for {
                _ <- Scenario.eval(m.chat.send(Strings.When).attempt)
                (hh, mm) <- Scenario.expect(
                  textMessage
                    .matching(Strings.WhenRegex)
                    .andThen(_.text.pipe { text =>
                      val s"$hh:$mm" = text
                      val hours = hh.toInt
                      val minutes = mm.toInt
                      (hours, minutes)
                    })
                )
                _ <- Scenario.eval(
                  SmokeService[F].create(sender, hh, mm)
                )
              } yield ()
            case _ => Scenario.done[F]
          }
          _ <- status match {
            case UserStatus.New | UserStatus.WaitingConfirmation => signUp
            case UserStatus.SignedIn | UserStatus.Admin          => program
            case UserStatus.Rejected =>
              Scenario.eval(m.chat.send(Strings.Rejected).void)
          }
        } yield ()
      }
    } yield ()

  def ioOp[F[_]: ContextShift, A](fa: F[A])(implicit blocker: Blocker): F[A] =
    ContextShift[F].blockOn(blocker)(fa)
}
