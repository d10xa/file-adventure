package ru.d10xa.file_adventure.fs

import java.nio.file.Path

import better.files.File
import cats.effect.Sync

trait Fs[F[_]] {
  def listRecursive(dir: Path, predicate: Path => Boolean): F[Vector[File]]
}
object Fs {
  def make[F[_]: Sync]: F[Fs[F]] =
    Sync[F].delay(new Fs[F] {
      override def listRecursive(
        dir: Path,
        predicate: Path => Boolean
      ): F[Vector[File]] = {
        val predFile: File => Boolean = f => predicate(f.path)
        Sync[F].delay(File(dir).list(predFile).toVector)
      }
    })
}
