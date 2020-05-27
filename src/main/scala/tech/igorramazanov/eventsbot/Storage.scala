package tech.igorramazanov.eventsbot

import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.{Files, Path, Paths, StandardOpenOption}
import java.time.{ZoneId, ZonedDateTime}

import canoe.api._
import canoe.syntax._
import canoe.models
import canoe.models.{Channel, Chat, Group, PrivateChat, Supergroup}
import cats.effect.Sync
import cats.implicits._
import simulacrum.typeclass
import Utils._

import scala.util.Try
import scala.util.chaining._

@typeclass trait Storage[F[_]] {
  def status(user: canoe.models.User): F[UserStatus]
  def save(chat: Chat, user: canoe.models.User, status: UserStatus): F[Unit]
  def delete(user: canoe.models.User): F[Unit]
  def restoreUsers: F[List[(Chat, canoe.models.User, UserStatus)]]
  def save(state: State[F]): F[Unit]
  def restoreState: F[State[F]]
  def adminChat: F[Chat]
  def adminId: F[Int]
}

object Storage {
  def create[F[_]: Sync: TelegramClient](
      usersFile: String,
      stateFile: String,
      zoneId: ZoneId
  ): F[Storage[F]] =
    Sync[F].delay {
      new Storage[F] {
        import canoe.models.Chat.chatDecoder
        import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
        import io.circe.parser._
        import io.circe.syntax._
        import io.circe.{Decoder, Encoder, Json}

        import scala.jdk.CollectionConverters._

        val usersPath: Path = Paths.get(usersFile)
        val statePath: Path = Paths.get(stateFile)
        val separator = "|||"
        val utf8: Charset = StandardCharsets.UTF_8

        val privateChatEncoder: Encoder[PrivateChat] =
          deriveEncoder[PrivateChat].mapJsonObject(
            _.add("type", Json.fromString("private"))
          )
        val groupEncoder: Encoder[Group] =
          deriveEncoder[Group].mapJsonObject(
            _.add("type", Json.fromString("group"))
          )
        val supergroupEncoder: Encoder[Supergroup] =
          deriveEncoder[Supergroup].mapJsonObject(
            _.add("type", Json.fromString("supergroup"))
          )
        val channelEncoder: Encoder[Channel] =
          deriveEncoder[Channel].mapJsonObject(
            _.add("type", Json.fromString("channel"))
          )

        implicit val chatEncoder: Encoder[Chat] = Encoder.instance[Chat] {
          case privateChat: PrivateChat => privateChatEncoder(privateChat)
          case group: Group             => groupEncoder(group)
          case supergroup: Supergroup   => supergroupEncoder(supergroup)
          case channel: Channel         => channelEncoder(channel)
        }

        implicit val userEncoder: Encoder[canoe.models.User] =
          deriveEncoder[canoe.models.User]

        implicit val userDecoder: Decoder[canoe.models.User] =
          deriveDecoder[canoe.models.User]

        override def status(user: models.User): F[UserStatus] =
          restoreUsers.map(_.collectFirst {
            case (_, u, status) if u.id === user.id => status
          }.getOrElse(UserStatus.New))

        override def save(
            chat: Chat,
            user: models.User,
            status: UserStatus
        ): F[Unit] =
          synchronized {
            this.restoreUsers.flatMap { list =>
              Sync[F].delay {
                val lines = list
                  .map {
                    case (c, u, _) if c.id === chat.id && u.id === user.id =>
                      (chat, user, status)
                    case tuple => tuple
                  }
                  .pipe(list =>
                    if (
                      list.exists {
                        case (c, u, _) => c.id === chat.id && u.id === user.id
                      }
                    ) list
                    else (chat, user, status) :: list
                  )
                  .map {
                    case (c, u, s) =>
                      c.asJson.noSpaces + separator + u.asJson.noSpaces + separator + s.code
                  }

                Files.write(
                  usersPath,
                  lines.asJava,
                  utf8,
                  StandardOpenOption.CREATE,
                  StandardOpenOption.TRUNCATE_EXISTING,
                  StandardOpenOption.WRITE
                )
              }
            }
          }

        override def delete(user: models.User): F[Unit] =
          Sync[F].delay {
            synchronized {
              val filteredOutUsers = Try(
                Files
                  .readAllLines(usersPath, utf8)
                  .asScala
                  .toList
                  .filter(!_.contains(user.id.toString))
              ).toOption.getOrElse(List.empty)
              Files.write(
                usersPath,
                filteredOutUsers.asJava,
                utf8,
                StandardOpenOption.CREATE,
                StandardOpenOption.WRITE,
                StandardOpenOption.TRUNCATE_EXISTING
              )
            }
          }

        override def save(state: State[F]): F[Unit] =
          Sync[F].delay {
            Files.write(
              statePath,
              List(State.encode(state)).asJava,
              utf8,
              StandardOpenOption.CREATE,
              StandardOpenOption.WRITE,
              StandardOpenOption.TRUNCATE_EXISTING
            )
          }

        override def restoreUsers: F[List[(Chat, models.User, UserStatus)]] =
          Sync[F]
            .delay {
              Files
                .readAllLines(usersPath, utf8)
                .asScala
                .toList
                .filter(_.nonEmpty)
                .flatMap { line =>
                  for {
                    (chatS, userS, statusS) <-
                      Try(
                        line.split(separator.toArray).filter(_.nonEmpty).toList
                      ).toOption
                        .map {
                          case List(chat, user, status) => (chat, user, status)
                        }
                    chatJson <- parse(chatS).toOption
                    chat <- chatJson.as[Chat].toOption
                    userJson <- parse(userS).toOption
                    user <- userJson.as[canoe.models.User].toOption
                    userStatus <- statusS.toIntOption.flatMap(UserStatus.apply)
                  } yield (chat, user, userStatus)
                }
            }

        override def restoreState: F[State[F]] =
          restoreUsers.map(_.flatMap {
            case (chat, user, UserStatus.SignedIn) =>
              User[F](user.id, user.fullName, s => chat.send(s).void).some
            case _ => None
          }) >>= { users =>
            Sync[F].delay {
              Try(
                State.decode[F](
                  users,
                  Files.readString(statePath)
                )
              ).toOption
                .getOrElse(
                  State(
                    None,
                    Map.empty,
                    Nil,
                    ZonedDateTime.now(zoneId),
                    past = true
                  )
                )
            }
          }

        override def adminChat: F[Chat] =
          restoreUsers.map(_.collectFirst {
            case (c, _, UserStatus.Admin) => c
          }) >>= (maybeAdminChat =>
            Sync[F].fromOption(
              maybeAdminChat,
              new IllegalStateException("Couldn't find admin")
            )
          )

        override def adminId: F[Int] =
          restoreUsers.map(_.collectFirst {
            case (_, u, UserStatus.Admin) => u.id
          }) >>= (maybeAdminId =>
            Sync[F].fromOption(
              maybeAdminId,
              new IllegalStateException("Couldn't find admin")
            )
          )
      }
    }
}
