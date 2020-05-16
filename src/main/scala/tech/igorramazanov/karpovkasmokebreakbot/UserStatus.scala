package tech.igorramazanov.karpovkasmokebreakbot

sealed trait UserStatus extends Product with Serializable {
  def code: Int
}
object UserStatus {
  final case object New extends UserStatus {
    val code = 0
  }
  final case object SignedIn extends UserStatus {
    val code = 1
  }
  final case object WaitingConfirmation extends UserStatus {
    val code = 2
  }
  final case object Rejected extends UserStatus {
    val code = 3
  }
  final case object Admin extends UserStatus {
    val code = 4
  }

  def apply(code: Int): Option[UserStatus] =
    code match {
      case 0 => Some(New)
      case 1 => Some(SignedIn)
      case 2 => Some(WaitingConfirmation)
      case 3 => Some(Rejected)
      case 4 => Some(Admin)
      case _ => None
    }
}
