package tech.igorramazanov.eventsbot

final case class User[F[_]](
    id: Int,
    fullName: String,
    callback: String => F[Unit]
)
