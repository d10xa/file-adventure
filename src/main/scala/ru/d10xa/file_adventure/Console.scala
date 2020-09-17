package ru.d10xa.file_adventure

import cats.effect.Sync

trait Console[F[_]] {
  def putStrLn(line: String): F[Unit]
}

object Console {
  def apply[F[_]](implicit console: Console[F]): Console[F] = console
  def make[F[_]: Sync]: F[Console[F]] =
    Sync[F].delay((line: String) => Sync[F].delay(println(line)))
}
