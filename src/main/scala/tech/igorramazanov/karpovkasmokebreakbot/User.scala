package tech.igorramazanov.karpovkasmokebreakbot

final case class User[F[_]](
    id: Int,
    fullName: String,
    callback: String => F[Unit]
)
