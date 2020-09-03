package tech.igorramazanov.eventsbot.storage

import java.time.ZonedDateTime

import doobie._
import tech.igorramazanov.eventsbot.model.{Event, Garden, User}

trait DoobieCodecs {

  implicit protected val readUser: Read[User] =
    Read[
      (
          Int,
          String,
          String,
          String,
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
          User.Status.withName(status),
          isParticipant
        )
    }

  implicit protected val readEvent: Read[Event] =
    Read[(Option[String], Double, Double, String, Boolean, Boolean)].map {
      case (description, longitude, latitude, when, past, needsReminding) =>
        Event(
          description,
          longitude,
          latitude,
          ZonedDateTime.parse(when),
          past,
          needsReminding
        )
    }

  implicit protected val readGarden: Read[Garden] =
    Read[(String, String)].map {
      case (lastWatering, nextWatering) =>
        Garden(
          ZonedDateTime.parse(lastWatering),
          ZonedDateTime.parse(nextWatering)
        )
    }
}
