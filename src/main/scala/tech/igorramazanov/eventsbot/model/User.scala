package tech.igorramazanov.eventsbot.model

final case class User(
    id: Int,
    firstName: String,
    username: String,
    status: User.Status.Status,
    isParticipant: Boolean
)

object User {
  object Status extends Enumeration {
    type Status = Value
    val New, SignedIn, WaitingConfirmation, Rejected, Admin,
        PersistentGardener =
      Value
  }
}
