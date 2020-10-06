package ru.d10xa.file_adventure

import java.nio.file.Path
import java.nio.file.Paths

import cats.Monad
import cats.implicits._
import ru.d10xa.file_adventure.core.Sha256Hash
import ru.d10xa.file_adventure.Main.sortHashes
import ru.d10xa.file_adventure.Main.sortedHashesToSingleHash
import ru.d10xa.file_adventure.core.FileAndHash
import ru.d10xa.file_adventure.fs.Checksum
import ru.d10xa.file_adventure.fs.Fs
import ru.d10xa.file_adventure.implicits._
import ru.d10xa.file_adventure.progress.TraverseProgress

class Sha256[F[_]: Fs: Monad: TraverseProgress: Checksum: Console]() {
  def run(c: Sha256Command): F[Unit] =
    Sha256
      .recursiveHash(Paths.get(c.dir))
      .map(_.show)
      .flatMap(Console[F].putStrLn)
}

object Sha256 {

  def filesToSingleHash[F[_]: Fs: Monad: TraverseProgress: Checksum](
    files: Vector[Path]
  ): F[Sha256Hash] =
    for {
      fileAndHashes <-
        files.traverseWithProgress((p: Path) => FileAndHash.fromFile[F](p))
      hashes = fileAndHashes.map(_.hash)
      sortedHashes = sortHashes(hashes)
      result = sortedHashesToSingleHash(sortedHashes)
    } yield result

  def recursiveHash[F[_]: Fs: Monad: TraverseProgress: Checksum](
    f: Path
  ): F[Sha256Hash] =
    for {
      files <- Fs[F].listRecursive(f, core.filePredicate)
      res <- filesToSingleHash[F](files)
    } yield res

}
