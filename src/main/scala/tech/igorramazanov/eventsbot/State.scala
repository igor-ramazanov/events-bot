package tech.igorramazanov.eventsbot

import java.time.ZonedDateTime

final case class State[F[_]](
    description: Option[String],
    longitude: Double,
    latitude: Double,
    participants: Map[Int, User[F]],
    all: List[User[F]],
    when: ZonedDateTime,
    past: Boolean
) {
  def save(user: User[F]): State[F] = {
    val ids = all.map(_.id).toSet
    if (ids(user.id))
      this
    else
      copy(all = user :: all)
  }
  def users: List[User[F]] =
    participants.values.toList
  def ids: List[Int] =
    participants.keys.toList
  def names: List[String] =
    participants.values.toList.map(_.fullName)
  def add(user: User[F]): State[F] =
    copy(participants = participants + (user.id -> user))
  def deleteParticipant(user: User[F]): State[F] =
    copy(participants = participants - user.id)
  def deleteCompletely(user: User[F]): State[F] =
    copy(
      participants = participants - user.id,
      all = all.filterNot(_.id == user.id)
    )
}

object State {
  def encode[F[_]](state: State[F]): String = {
    val description = state.description.getOrElse("").filter(_ != '|').trim
    val longitude = state.longitude.toString
    val latitude = state.latitude.toString
    val ids = state.ids.mkString(",")
    val when = state.when.toString
    val past = state.past.toString
    s"$description|$longitude|$latitude|$ids|$when|$past"
  }

  def decode[F[_]](all: List[User[F]], s: String): State[F] = {
    val Array(description, longitude, latitude, ids, when, past) = s.split('|')
    State(
      description = Option(description).filter(_.nonEmpty),
      longitude = longitude.toDouble,
      latitude = latitude.toDouble,
      participants = ids
        .split(',')
        .map(_.toInt)
        .flatMap(id => all.find(_.id == id).map(id -> _))
        .toMap,
      all = all,
      when = ZonedDateTime.parse(when),
      past = past == "true"
    )
  }
}
