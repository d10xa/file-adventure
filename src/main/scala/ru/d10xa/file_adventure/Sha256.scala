package ru.d10xa.file_adventure

import better.files._
import cats.effect.Sync
import cats.implicits._
import ru.d10xa.file_adventure.core.Sha256Hash
import ru.d10xa.file_adventure.Main.sortHashes
import ru.d10xa.file_adventure.Main.sortedHashesToSingleHash
import ru.d10xa.file_adventure.Progress.ProgressBuilder
import ru.d10xa.file_adventure.fs.Checksum

class Sha256[F[_]: Sync: Checksum: Console](
  progressBuilder: ProgressBuilder[F]
) {
  def run(c: Sha256Command): F[Unit] =
    Sha256
      .recursiveHash(progressBuilder, File(c.dir))
      .map(_.show)
      .flatMap(Console[F].putStrLn)
}

object Sha256 {

  def filesToSingleHash[F[_]: Sync: Checksum](
    progressBuilder: ProgressBuilder[F],
    files: Vector[File]
  ): F[Sha256Hash] =
    for {
      fileAndHashes <-
        core.filesToHashesWithProgressBar[F](progressBuilder, files)
      hashes = fileAndHashes.map(_.hash)
      sortedHashes = sortHashes(hashes)
      result = sortedHashesToSingleHash(sortedHashes)
    } yield result

  def recursiveHash[F[_]: Sync: Checksum](
    progressBuilder: ProgressBuilder[F],
    f: File
  ): F[Sha256Hash] =
    filesToSingleHash(
      progressBuilder,
      f.list(core.filePredicate).toList.toVector
    )

}
