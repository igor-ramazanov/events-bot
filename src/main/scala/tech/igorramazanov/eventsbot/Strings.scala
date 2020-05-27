package tech.igorramazanov.eventsbot

import cats.Show
import cats.syntax.show._

object Strings {
  import Utils.showForZonedDateTime
  implicit def showState[F[_]]: Show[State[F]] =
    Show.show(state =>
      if (state.past)
        s"Она уже прошла :( Создашь новую? /create"
      else
        s"Время следующей встречи: ${state.when.show}. Участники: ${state.names
          .mkString(", ")}"
    )

  def Created[F[_]](creatingUser: String, state: State[F]): String =
    s"$creatingUser создал(а) новую встречу в ${state.when.show}."

  val SignUp: String =
    "Нажмите на кнопку, чтобы зарегистрироваться и админ проверит вашу заявку.\nДля проверки необходимо иметь публичный @username."
  def Request(username: String, id: Int): String =
    s"Новая заявка - @$username. /yes_$id или /no_$id?"
  val Approved: String = "Вы зарегистрированы, добро пожаловать!"
  val MustHavePublicUsername: String =
    "Для проверки необходимо иметь публичный @username."
  val SignedUp: String =
    "Ура! Ваша заявка подтверждена и теперь вы можете ходить с нами на встречи :)"
  val Rejected: String =
    "Увы, но админ решил не добавлять вас :("
  val Greeting: String =
    """|Привет!
       |/show - Показать информацию о следующей встрече
       |/leave - Покинуть запланированную встречу
       |/delete - Удалить свой аккаунт из системы
       |/join - Присоединиться к запланированной встрече
       |/create - Запланировать новую встречу, старая при этом отменяется
       |/help - Показать эту помощь
       |/feedback - Оставить предложения об улучшении
       |""".stripMargin
  val Feedback: String =
    "Благодарю за неравнодушие :) Можете начать писать сообщение, а бот переотправит его мне."
  val FeedbackConfirmation: String =
    "Спасибо вам! Я прочитаю и поразмышляю о ваших предложениях."
  val Deleted =
    "Ваш аккаунт был удален, вы больше не будете получать уведомлений.\nДля того, чтобы зарегистрироваться обратно, введите любую команду кроме /delete."
  val FullConfirmation =
    "Отлично! Я напомню о встрече заранее за 5 минут и в момент ее начала. "
  val ShortConfirmation =
    "Отлично! Я напомню о встрече в момент его начала. "
  val When =
    "Во сколько? Я понимаю следующий формат hh:mm, например 20:45 или 00:05."
  val Left =
    "Вы передумали идти на встречу и были удалены из списка участников. "
  val NotExisted = "Вы и так не собирались идти на эту встречу. "
  def LeftNotifyPeers[F[_]](leavingUserFullName: String, state: State[F]) =
    s"$leavingUserFullName передумал(а) идти на встречу. Участники: ${state.names.mkString(", ")}"
  def JoinedNotifyPeers[F[_]](joiningUserFullName: String, state: State[F]) =
    s"$joiningUserFullName решил(а) присоединиться к встрече. Участники: ${state.names.mkString(", ")}"
  val WhenRegex = "([01][0-9]|2[0-3]):([0-5][0-9])"
  val Reminder = "Встречаемся через 5 минут! "
  def Start[F[_]](state: State[F]) =
    s"""|Время встречи, встречаемся внизу.
       |И, ребята, я крайне рекомендую приходить в масках, чтобы минизировать вероятность заражения, пока люди все еще болеют коронавирусом.
       |Участники: ${state.names.mkString(", ")}""".stripMargin
}
