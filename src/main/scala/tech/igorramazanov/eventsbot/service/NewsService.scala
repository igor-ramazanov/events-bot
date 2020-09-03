package tech.igorramazanov.eventsbot.service

import canoe.api._
import canoe.syntax._
import cats.implicits._
import org.slf4j.LoggerFactory
import simulacrum.typeclass
import tech.igorramazanov.eventsbot.Utils.{user2chatApi, MonadThrowable}
import tech.igorramazanov.eventsbot.storage.Storage

@typeclass trait NewsService[F[_]] {
  def post(news: String): F[Unit]
}

object NewsService {
  def create[F[_]: TelegramClient: Storage: MonadThrowable]: NewsService[F] =
    new NewsService[F] {
      private val logger = LoggerFactory.getLogger(getClass)
      override def post(news: String): F[Unit] =
        for {
          users <- Storage[F].users
          // TODO: Sometimes fails for unknown reasons
          _ <- users.traverse { u =>
            u.send(news)
              .attempt
              .map(_.fold(e => logger.error(e.getMessage, e), _ => ()))
          }
        } yield ()
    }
}
