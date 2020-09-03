package tech.igorramazanov.eventsbot.ui.i18n

import java.time.format.DateTimeFormatter

import tech.igorramazanov.eventsbot.model._

trait I18N {
  val whenRegex = "([01][0-9]|2[0-3]):([0-5][0-9])"
  def event(event: Event, ps: List[User]): String
  def created(originator: String, event: Event): String
  def signUp: String
  def request(username: String, id: Int): String
  def approved: String
  def publicUsernameRequirement: String
  def signedUp: String
  def rejected: String
  def greeting: String
  def feedback: String
  def feedbackConfirmation: String
  def deleted: String
  def schedulingConfirmationFull: String
  def schedulingConfirmationShort: String
  def description: String
  def when: String
  def where: String
  def left: String
  def didNotExist: String
  def notifyLeaving(username: String, participants: List[User]): String
  def notifyJoining(username: String, participants: List[User]): String
  def reminder: String
  def start(event: Event, participants: List[User]): String
  def newsPrepare: String
  def gardenNewcomer: String
}

object I18N {
  object Russian extends I18N {

    def event(event: Event, participants: List[User]): String =
      if (event.past)
        s"Она уже прошла :( Создашь новую? /create"
      else {
        val when = event.when.format(DateTimeFormatter.ofPattern("HH:mm"))
        val description =
          event.description.fold("Встреча на свободную тему.")("Описание: " + _)
        val names = participants.map(_.username).mkString(", ")
        s"""|Время следующей встречи: $when.
           |Участники: $names.
           |$description""".stripMargin
      }

    def created(originator: String, event: Event): String =
      s"$originator создал(а) новую встречу в ${event.when.format(DateTimeFormatter.ofPattern("HH:mm"))}. Подробнее: /show"

    val signUp: String =
      "Нажмите на кнопку, чтобы зарегистрироваться и админ проверит вашу заявку.\nДля проверки необходимо иметь публичный @username."

    def request(username: String, id: Int): String =
      s"Новая заявка - @$username. /yes_$id или /no_$id?"

    val approved: String = "Вы зарегистрированы, добро пожаловать!"

    val publicUsernameRequirement: String =
      "Для проверки необходимо иметь публичный @username."

    val signedUp: String =
      "Ура! Ваша заявка подтверждена и теперь вы можете ходить с нами на встречи :)"

    val rejected: String =
      "Увы, но админ решил не добавлять вас :("

    val greeting: String =
      """|Привет!
         |/show - Показать информацию о следующей встрече
         |/join - Присоединиться к запланированной встрече
         |/help - Показать эту помощь
         |/feedback - Оставить предложения об улучшении
         |/delete - Удалить свой аккаунт из системы
         |/garden - Показать информацию об общественном огороде на Карповке
         |/leave - Покинуть запланированную встречу
         |/create - Запланировать новую встречу, старая при этом отменяется
         |""".stripMargin

    val feedback: String =
      "Благодарю за неравнодушие :) Можешь начать писать сообщение, а бот переотправит его мне."

    val feedbackConfirmation: String =
      "Спасибо вам! Я прочитаю и поразмышляю о ваших предложениях."

    val deleted =
      "Ваш аккаунт был удален, вы больше не будете получать уведомлений.\nДля того, чтобы зарегистрироваться обратно, введите любую команду кроме /delete."

    val schedulingConfirmationFull =
      "Отлично!\nЯ напомню о встрече заранее за 5 минут и в момент ее начала. "

    val schedulingConfirmationShort =
      "Отлично!\nЯ напомню о встрече в момент его начала. "

    val description =
      "Укажите тематику встречи (если не хотите - нажмите на кнопку снизу)."

    val when =
      "Во сколько? Я понимаю следующий формат hh:mm, например 20:45 или 00:05."

    val where = "Укажите место встречи."

    val left =
      "Вы передумали идти на встречу и были удалены из списка участников. "

    val didNotExist = "Вы и так не собирались идти на эту встречу. "

    def notifyLeaving(leavingUsername: String, participants: List[User]) =
      s"$leavingUsername передумал(а) идти на встречу. Участники: ${participants.map(_.username).mkString(", ")}"

    def notifyJoining(
        joiningUsername: String,
        participants: List[User]
    ) =
      s"$joiningUsername решил(а) присоединиться к встрече. Участники: ${participants.map(_.username).mkString(", ")}"

    val reminder = "Встречаемся через 5 минут! "

    def start(event: Event, participants: List[User]): String = {
      val names = participants.map(_.username).mkString
      val description =
        event.description.fold("Встреча на свободную тему.")("Описание: " + _)
      s"""|Итак, время встречи.
         |И, ребята, я крайне рекомендую приходить в масках, чтобы минизировать вероятность заражения, пока люди все еще болеют коронавирусом.
         |Участники: $names
         |$description""".stripMargin
    }

    val newsPrepare: String =
      """|Теперь как админ вы можете написать и отправить новость для участников бота.
         |Либо ввести /start для возврата в предыдущее меню.""".stripMargin

    val gardenNewcomer: String =
      """|ВНИМАНИЕ: Этот функционал находится в стадии активной разработки, нерабочие команды помечены как 'TODO'.
         |
         |Пару лет назад по инициативе жителей и активистов в парке на набережной реки Карповки был создан общественный огород - https://vk.com/ogorodnakarpovke
         |
         |В этом разделе вы можете предложить свою помощь в развитии огорода.
         |
         |Что вам более интересно? (Можно поменять в будущем):
         |/persistent - TODO Хочу помогать постоянно
         |/now - TODO Хочу помочь сейчас
         |/start - Предыдущее меню""".stripMargin
  }
}
