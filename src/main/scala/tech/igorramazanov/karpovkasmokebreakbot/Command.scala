package tech.igorramazanov.karpovkasmokebreakbot

import cats.instances.string._
import cats.syntax.show._
import cats.syntax.eq._

object Command extends Enumeration {
  type Command = Value
  val Delete, Start, Show, Help, Join, Leave, Create, Signup = Value

  implicit val commandShow: cats.Show[Command] =
    cats.Show.show(c => "/" + c.toString.toLowerCase)

  implicit val commandEq: cats.Eq[Command] =
    cats.Eq.instance((a, b) => a.show === b.show)
}
