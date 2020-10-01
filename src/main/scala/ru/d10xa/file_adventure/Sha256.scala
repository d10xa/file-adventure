package ru.d10xa.file_adventure

import java.nio.file.Path
import java.nio.file.Paths

import cats.FlatMap
import cats.Functor
import cats.effect.Sync
import cats.implicits._
import ru.d10xa.file_adventure.core.Sha256Hash
import ru.d10xa.file_adventure.Main.sortHashes
import ru.d10xa.file_adventure.Main.sortedHashesToSingleHash
import ru.d10xa.file_adventure.core.FileAndHash
import ru.d10xa.file_adventure.fs.Checksum
import ru.d10xa.file_adventure.fs.Fs
import ru.d10xa.file_adventure.progress.Progress.ProgressBuilder
import ru.d10xa.file_adventure.progress.TraverseProgress._
import ru.d10xa.file_adventure.implicits._

class Sha256[F[_]: Sync: Functor: FlatMap: ProgressBuilder: Checksum: Console](
  fs: Fs[F]
) {
  def run(c: Sha256Command): F[Unit] =
    Sha256
      .recursiveHash(fs, Paths.get(c.dir))
      .map(_.show)
      .flatMap(Console[F].putStrLn)
}

object Sha256 {

  def filesToSingleHash[F[_]: Sync: Functor: ProgressBuilder: Checksum](
    files: Vector[Path]
  ): F[Sha256Hash] =
    for {
      fileAndHashes <- files.traverseWithProgress(FileAndHash.fromFile[F](_))
      hashes = fileAndHashes.map(_.hash)
      sortedHashes = sortHashes(hashes)
      result = sortedHashesToSingleHash(sortedHashes)
    } yield result

  def recursiveHash[F[_]: Sync: FlatMap: ProgressBuilder: Checksum](
    fs: Fs[F],
    f: Path
  ): F[Sha256Hash] =
    for {
      files <- fs.listRecursive(f, core.filePredicate)
      res <- filesToSingleHash(files)
    } yield res

}
