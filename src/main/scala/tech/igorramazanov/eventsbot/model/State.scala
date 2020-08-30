package tech.igorramazanov.eventsbot.model

import java.time.ZonedDateTime

case class State(
    description: Option[String],
    longitude: Double,
    latitude: Double,
    when: ZonedDateTime,
    past: Boolean
)
