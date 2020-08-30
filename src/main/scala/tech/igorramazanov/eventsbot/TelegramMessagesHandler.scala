package tech.igorramazanov.eventsbot

import canoe.api._
import canoe.api.models.Keyboard
import canoe.methods.messages.ForwardMessage
import canoe.models.messages.{TelegramMessage, TextMessage}
import canoe.models.{KeyboardButton, ReplyKeyboardMarkup}
import canoe.syntax._
import cats.effect._
import cats.effect.concurrent.MVar
import cats.implicits._
import tech.igorramazanov.eventsbot.Utils.{ioOp, _}
import tech.igorramazanov.eventsbot.i18n.Language
import tech.igorramazanov.eventsbot.model.Command.commandShow
import tech.igorramazanov.eventsbot.model._
import tech.igorramazanov.eventsbot.storage.Storage

import scala.concurrent.duration._
import scala.util.chaining._

object TelegramMessagesHandler {
  private val pollingPeriod: FiniteDuration = 1.second

  def handlers[F[
      _
  ]: TelegramClient: MonadThrowable: Storage: ContextShift: Timer: EventService](
      language: Language,
      channel: MVar[F, (Boolean, Int)]
  )(implicit blocker: Blocker): List[Scenario[F, Unit]] =
    List(approvals[F](channel), handleUserCommands[F](language, channel))

  private def approvals[F[
      _
  ]: TelegramClient: MonadThrowable: Storage: ContextShift](
      channel: MVar[F, (Boolean, Int)]
  )(implicit blocker: Blocker): Scenario[F, Unit] =
    for {
      adminId <- Scenario.eval(ioOp(Storage[F].admin)).map(_.id)
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

  private def validCommands: Expect[TextMessage] =
    textMessage.matching(Command.values.map(_.show).mkString("|"))

  private def handleUserCommands[F[
      _
  ]: Timer: MonadThrowable: Storage: EventService: ContextShift](
      language: Language,
      channel: MVar[F, (Boolean, Int)]
  )(implicit
      telegramClient: TelegramClient[F],
      blocker: Blocker
  ): Scenario[F, Unit] =
    for {
      m <- Scenario.expect(validCommands)
      _ <- m.from.fold(Scenario.done[F]) { u =>
        for {
          sender <-
            Scenario
              .eval(Storage[F].user(u.id))
              .map(
                _.getOrElse(
                  User(
                    u.id,
                    u.firstName,
                    u.username.getOrElse(""),
                    User.Status.New,
                    isParticipant = false
                  )
                )
              )
          isAdmin <-
            Scenario
              .eval(ioOp(Storage[F].admin))
              .map(admin => sender.id === admin.id)
          start =
            m.chat
              .send(
                language.greeting,
                keyboard = Keyboard.Reply(
                  ReplyKeyboardMarkup(
                    List(
                      List(
                        KeyboardButton.text(Command.Show.show),
                        KeyboardButton.text(Command.Join.show),
                        KeyboardButton.text(Command.Help.show)
                      ),
                      List(
                        KeyboardButton.text(Command.Feedback.show),
                        KeyboardButton.text(Command.Delete.show),
                        KeyboardButton.text(Command.Garden.show)
                      ),
                      List(
                        KeyboardButton.text(Command.Leave.show),
                        KeyboardButton.text(Command.Create.show)
                      ) ++ (if (isAdmin)
                              List(KeyboardButton.text(Command.News.show))
                            else Nil)
                    ),
                    resizeKeyboard = true.some
                  )
                )
              )
              .attempt
              .void
          signUp = u.username.fold(
            Scenario.eval(m.chat.send(language.publicUsernameRequirement).void)
          ) { username =>
            sender.status match {
              case User.Status.New =>
                Scenario
                  .eval(
                    m.chat.send(
                      language.signUp,
                      keyboard = Keyboard.Reply(
                        ReplyKeyboardMarkup.singleButton(
                          KeyboardButton.text(Command.Signup.show),
                          resizeKeyboard = true.some
                        )
                      )
                    ) >> ioOp(
                      Storage[F]
                        .save(
                          sender.copy(status = User.Status.WaitingConfirmation)
                        )
                    )
                  )
              case User.Status.WaitingConfirmation
                  if Command
                    .withName(m.text.tail.capitalize) === Command.Signup =>
                for {
                  admin <- Scenario.eval(ioOp(Storage[F].admin))
                  _ <-
                    Scenario.eval(admin.send(language.request(username, u.id)))
                  signedUp <- Scenario.eval(
                    (Timer[F].sleep(pollingPeriod) >> channel.read)
                      .iterateUntil(_._2 === u.id)
                      .map(_._1)
                  )
                  _ <- Scenario.eval(channel.take)
                  _ <-
                    if (signedUp)
                      Scenario.eval(for {
                        _ <- ioOp(
                          Storage[F].save(
                            sender.copy(status = User.Status.SignedIn)
                          )
                        )
                        _ <-
                          EventService[F]
                            .save(sender)
                        _ <-
                          m.chat
                            .send(language.signedUp)
                        _ <- EventService[F].show(sender)
                        _ <- start
                      } yield ())
                    else
                      Scenario.eval(
                        ioOp(
                          Storage[F].save(
                            sender.copy(status = User.Status.Rejected)
                          )
                        ) >> m.chat.send(language.rejected)
                      )
                } yield ()
              case _ => Scenario.done[F]
            }
          }

          program = Command.withName(m.text.tail.capitalize) match {
            case Command.Show =>
              Scenario.eval(EventService[F].show(sender))
            case Command.Join =>
              Scenario.eval(EventService[F].join(sender))
            case Command.Start | Command.Help =>
              Scenario.eval(start)
            case Command.Feedback =>
              for {
                _ <- Scenario.eval(m.chat.send(language.feedback))
                feedback <- Scenario.expect(
                  PartialFunction
                    .fromFunction(identity[TelegramMessage])
                )
                admin <- Scenario.eval(ioOp(Storage[F].admin))
                _ <- Scenario.eval(
                  telegramClient.execute(
                    ForwardMessage(
                      admin.id,
                      feedback.chat.id,
                      feedback.messageId
                    )
                  )
                )
                _ <- Scenario.eval(m.chat.send(language.feedbackConfirmation))
              } yield ()
            case Command.Delete =>
              Scenario
                .eval(
                  EventService[F].delete(sender) >>
                    ioOp(ioOp(Storage[F].delete(u.id))) >>
                    m.chat.send(language.deleted).void
                )
            case Command.Garden =>
              for {
                _ <- Scenario.eval(
                  m.chat
                    .send(
                      language.gardenNewcomer,
                      keyboard = Keyboard.Reply(
                        ReplyKeyboardMarkup(
                          List(
                            List(
                              KeyboardButton.text(Command.Persistent.show),
                              KeyboardButton.text(Command.Now.show),
                              KeyboardButton.text(Command.Start.show)
                            )
                          ),
                          resizeKeyboard = true.some
                        )
                      )
                    )
                    .attempt
                )
              } yield ()
            case Command.Leave =>
              Scenario.eval(EventService[F].leave(sender))
            case Command.Create =>
              for {
                _ <- Scenario.eval(m.chat.send(language.when).attempt)
                (hh, mm) <- Scenario.expect(
                  textMessage
                    .matching(language.whenRegex)
                    .andThen(_.text.pipe { text =>
                      val s"$hh:$mm" = text
                      val hours = hh.toInt
                      val minutes = mm.toInt
                      (hours, minutes)
                    })
                )
                _ <- Scenario.eval(
                  m.chat.send(
                    language.description,
                    keyboard = Keyboard.Reply(
                      ReplyKeyboardMarkup.singleButton(
                        KeyboardButton.text("/no_descriprion")
                      )
                    )
                  )
                )
                maybeDescription <- Scenario.expect(
                  text
                    .andThen(_.trim)
                    .andThen({
                      case "/no_descriprion" => none[String]
                      case s                 => s.some
                    })
                )
                _ <- Scenario.eval(m.chat.send(language.where).attempt)
                place <- Scenario.expect(location)
                _ <- Scenario.eval(
                  EventService[F]
                    .create(
                      sender,
                      maybeDescription,
                      hh,
                      mm,
                      place.longitude,
                      place.latitude
                    )
                )
                _ <- Scenario.eval(start)
              } yield ()
            case Command.News if isAdmin =>
              for {
                _ <- Scenario.eval(
                  m.chat.send(
                    language.newsPrepare,
                    keyboard = Keyboard.Reply(
                      ReplyKeyboardMarkup(
                        List(
                          List(
                            KeyboardButton
                              .text(Command.Start.show)
                          )
                        )
                      )
                    )
                  )
                )
                t <- Scenario.expect(text)
                _ <-
                  Scenario
                    .eval(EventService[F].post(t))
              } yield ()
            case _ => Scenario.done[F]
          }
          _ <- sender.status match {
            case User.Status.New | User.Status.WaitingConfirmation =>
              signUp
            case User.Status.SignedIn | User.Status.Admin => program
            case User.Status.Rejected =>
              Scenario.eval(m.chat.send(language.rejected).void)
            case _ => Scenario.done[F]
          }
        } yield ()
      }
    } yield ()
}
