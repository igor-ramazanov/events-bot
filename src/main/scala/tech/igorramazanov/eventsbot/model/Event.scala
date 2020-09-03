package tech.igorramazanov.eventsbot.model

import java.time.ZonedDateTime

final case class Event(
    description: Option[String],
    longitude: Double,
    latitude: Double,
    when: ZonedDateTime,
    past: Boolean,
    needsReminding: Boolean
)
