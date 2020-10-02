package ru.d10xa.file_adventure

import java.nio.file.Path
import java.nio.file.Paths

import cats.FlatMap
import cats.Functor
import cats.effect.Bracket
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

class Sha256[F[_]: Fs: Bracket[
  *[_],
  Throwable
]: Functor: FlatMap: ProgressBuilder: Checksum: Console]() {
  def run(c: Sha256Command): F[Unit] =
    Sha256
      .recursiveHash(Paths.get(c.dir))
      .map(_.show)
      .flatMap(Console[F].putStrLn)
}

object Sha256 {

  def filesToSingleHash[
    F[_]: Fs: FlatMap: Functor: ProgressBuilder: Checksum: Bracket[*[
      _
    ], Throwable]
  ](
    files: Vector[Path]
  ): F[Sha256Hash] =
    for {
      fileAndHashes <-
        files.traverseWithProgress((p: Path) => FileAndHash.fromFile[F](p))
      hashes = fileAndHashes.map(_.hash)
      sortedHashes = sortHashes(hashes)
      result = sortedHashesToSingleHash(sortedHashes)
    } yield result

  def recursiveHash[F[_]: Bracket[
    *[_],
    Throwable
  ]: Fs: FlatMap: ProgressBuilder: Checksum](
    f: Path
  ): F[Sha256Hash] =
    for {
      files <- Fs[F].listRecursive(f, core.filePredicate)
      res <- filesToSingleHash[F](files)
    } yield res

}
