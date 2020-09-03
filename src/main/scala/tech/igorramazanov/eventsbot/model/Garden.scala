package tech.igorramazanov.eventsbot.model

import java.time.ZonedDateTime

final case class Garden(
    lastWatering: ZonedDateTime,
    nextWatering: ZonedDateTime
)
