package ru.d10xa.file_adventure

import cats.effect.Sync
import ru.d10xa.file_adventure.fs.Checksum
import ru.d10xa.file_adventure.fs.FileWrite
import cats.implicits._
import ru.d10xa.file_adventure.Progress.ProgressBuilder
import ru.d10xa.file_adventure.fs.Fs

class Context[F[_]](
  create: Create[F],
  check: Check[F],
  sha256: Sha256[F],
  minus: Minus[F]
) {
  def run(cb: CommandBase): F[Unit] =
    cb match {
      case c: MinusCommand => minus.run(c)
      case c: Sha256Command => sha256.run(c)
      case c: CreateCommand => create.run(c)
      case c: CheckCommand => check.run(c)
    }
}

object Context {
  def make[F[_]: Sync]: F[Context[F]] =
    for {
      implicit0(checksum: Checksum[F]) <- Checksum.make[F]
      implicit0(fileWrite: FileWrite[F]) <- FileWrite.make[F]
      implicit0(console: Console[F]) <- Console.make[F]
      progressBuilder: ProgressBuilder[F] <- Progress.builder[F]
      fs <- Fs.make[F]
      create = new Create[F](fs, progressBuilder)
      check = new Check[F](fs) // TODO progress
      sha256 = new Sha256[F](fs, progressBuilder)
      minus = new Minus[F](fs, progressBuilder)
    } yield new Context[F](create, check, sha256, minus)
}
