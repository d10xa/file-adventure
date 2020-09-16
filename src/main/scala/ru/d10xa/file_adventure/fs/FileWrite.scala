package ru.d10xa.file_adventure.fs

import java.nio.file.Path

import cats.effect.Sync
import cats.implicits._

trait FileWrite[F[_]] {
  def writeString(file: Path, content: String): F[Unit]
}

object FileWrite {
  def apply[F[_]](implicit fileWrite: FileWrite[F]): FileWrite[F] =
    fileWrite

  def make[F[_]: Sync]: F[FileWrite[F]] =
    Sync[F].delay((file: Path, content: String) =>
      Sync[F].delay(better.files.File(file).write(content)).void
    )
}
