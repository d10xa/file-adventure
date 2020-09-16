package ru.d10xa.file_adventure.fs

import java.nio.file.Path

import cats.effect.Sync
import ru.d10xa.file_adventure.core.Sha256Hash

trait Checksum[F[_]] {
  def sha256(file: Path): F[Sha256Hash]
}
object Checksum {
  def apply[F[_]](implicit checksum: Checksum[F]): Checksum[F] = checksum
  def make[F[_]: Sync]: F[Checksum[F]] =
    Sync[F].delay((file: Path) =>
      Sync[F].delay(Sha256Hash(better.files.File(file).sha256.toLowerCase))
    )
}
