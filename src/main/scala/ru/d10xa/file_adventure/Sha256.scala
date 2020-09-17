package ru.d10xa.file_adventure

import better.files._
import cats.effect.Sync
import cats.implicits._
import ru.d10xa.file_adventure.core.Sha256Hash
import ru.d10xa.file_adventure.Main.sortHashes
import ru.d10xa.file_adventure.Main.sortedHashesToSingleHash
import ru.d10xa.file_adventure.fs.Checksum

class Sha256[F[_]: Sync: Checksum: Console] {
  def run(c: Sha256Command): F[Unit] =
    Sha256.recursiveHash(File(c.dir)).map(_.show).flatMap(Console[F].putStrLn)
}

object Sha256 {

  def filesToSingleHash[F[_]: Sync: Checksum](
    files: Vector[File]
  ): F[Sha256Hash] =
    for {
      fileAndHashes <- core.filesToHashesWithProgressBar[F](files)
      hashes = fileAndHashes.map(_.hash)
      sortedHashes = sortHashes(hashes)
      result = sortedHashesToSingleHash(sortedHashes)
    } yield result

  def recursiveHash[F[_]: Sync: Checksum](f: File): F[Sha256Hash] =
    filesToSingleHash(f.list(core.filePredicate).toList.toVector)

}
