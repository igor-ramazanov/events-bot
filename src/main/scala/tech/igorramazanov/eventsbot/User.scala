package tech.igorramazanov.eventsbot

final case class User[F[_]](
    id: Int,
    fullName: String,
    sendString: String => F[Unit],
    sendLocation: (Double, Double) => F[Unit]
)
