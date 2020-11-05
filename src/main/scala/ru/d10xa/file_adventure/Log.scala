package ru.d10xa.file_adventure

import cats.Applicative

trait Log[F[_]] {
  def info(msg: String): F[Unit]
  def debug(msg: => String): F[Unit]
}

object Log {
  def apply[F[_]](implicit instance: Log[F]): Log[F] = instance
  def make[F[_]: Console: Applicative](debug: Boolean): Log[F] =
    if (debug) new LogDebug[F] else new LogInfo[F]

  private[Log] class LogInfo[F[_]: Console: Applicative] extends Log[F] {
    override def info(msg: String): F[Unit] = Console[F].putStrLn(msg)
    override def debug(msg: => String): F[Unit] = Applicative[F].unit
  }
  private[Log] class LogDebug[F[_]: Console] extends Log[F] {
    override def info(msg: String): F[Unit] = Console[F].putStrLn(msg)
    override def debug(msg: => String): F[Unit] =
      Console[F].putStrLn(s"DEBUG: $msg")
  }
}
