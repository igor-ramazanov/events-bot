package tech.igorramazanov.karpovkasmokebreakbot

import cats.Show
import cats.syntax.show._

object Strings {
  import Utils.showForZonedDateTime
  implicit def showState[F[_]]: Show[State[F]] =
    Show.show(state =>
      if (state.past)
        s"Он уже прошел :( Создашь новый? /create"
      else
        s"Время следующего перекура: ${state.when.show}. Участники: ${state.names
          .mkString(", ")}"
    )

  def Created[F[_]](creatingUser: String, state: State[F]): String =
    s"$creatingUser создал(а) новый перекур в ${state.when.show}."

  val SignUp: String =
    "Нажмите на кнопку, чтобы зарегистрироваться и админ проверит вашу заявку.\nДля проверки необходимо иметь публичный @username."
  def Request(username: String, id: Int): String =
    s"Новая заявка - @$username. /yes_$id или /no_$id?"
  val Approved: String = "Вы зарегистрированы, добро пожаловать!"
  val MustHavePublicUsername: String =
    "Для проверки необходимо иметь публичный @username."
  val SignedUp: String =
    "Ура! Ваша заявка подтверждена и теперь вы можете ходить с нами на перекуры :)"
  val Rejected: String =
    "Увы, но админ решил не добавлять вас :("
  val Greeting: String =
    """|Привет!
       |/show - Показать информацию о следующем перекуре
       |/leave - Покинуть запланированный перекур
       |/delete - Удалить свой аккаунт из системы
       |/join - Присоединиться к запланированному перекуру
       |/create - Запланировать новый перекур, старый при этом отменяется
       |/help - Показать эту помощь
       |""".stripMargin
  val Deleted =
    "Ваш аккаунт был удален, вы больше не будете получать уведомлений.\nДля того, чтобы зарегистрироваться обратно, введите любую команду кроме /delete."
  val FullConfirmation =
    "Отлично! Я напомню о перекуре заранее за 5 минут и в момент его начала. "
  val ShortConfirmation =
    "Отлично! Я напомню о перекуре в момент его начала. "
  val When =
    "Во сколько? Я понимаю следующий формат hh:mm, например 20:45 или 00:05."
  val Left =
    "Вы передумали идти на перекур и были удалены из списка участников. "
  val NotExisted = "Вы и так не собирались идти на этот перекур. "
  def LeftNotifyPeers[F[_]](leavingUserFullName: String, state: State[F]) =
    s"$leavingUserFullName передумал(а) идти на перекур. Участники: ${state.names.mkString(", ")}"
  def JoinedNotifyPeers[F[_]](joiningUserFullName: String, state: State[F]) =
    s"$joiningUserFullName решил(а) присоединиться к перекуру. Участники: ${state.names.mkString(", ")}"
  val WhenRegex = "([01][0-9]|2[0-3]):([0-5][0-9])"
  val Reminder = "Перекур через 5 минут. "
  val Start = "Время перекура, встречаемся внизу. "
}
