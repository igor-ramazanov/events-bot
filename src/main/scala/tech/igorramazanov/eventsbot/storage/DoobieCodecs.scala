package tech.igorramazanov.eventsbot.storage

import java.time.ZonedDateTime

import doobie._
import tech.igorramazanov.eventsbot.model.{State, User}

trait DoobieCodecs {
  implicit protected val readUser: Read[User] =
    Read[
      (
          Int,
          String,
          String,
          Int,
          Boolean
      )
    ].map {
      case (
            id,
            firstName,
            username,
            status,
            isParticipant
          ) =>
        User(
          id,
          firstName,
          username,
          User.Status(status),
          isParticipant
        )
    }
  implicit protected val readState: Read[State] =
    Read[(Option[String], Double, Double, String, Boolean)].map {
      case (description, longitude, latitude, when, past) =>
        State(
          description,
          longitude,
          latitude,
          ZonedDateTime.parse(when),
          past
        )
    }
}
