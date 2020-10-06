package ru.d10xa.file_adventure

import java.nio.file.Path
import java.nio.file.Paths

import ru.d10xa.file_adventure.core.FileAndHash
import cats._
import cats.implicits._
import ru.d10xa.file_adventure.fs.Checksum
import ru.d10xa.file_adventure.fs.Fs
import ru.d10xa.file_adventure.implicits._
import ru.d10xa.file_adventure.progress.TraverseProgress

class Minus[
  F[_]: Fs: TraverseProgress: Monad: Checksum: Console
] {

  def run(c: MinusCommand): F[Unit] =
    for {
      files <- minus(Paths.get(c.left), Paths.get(c.right))
      strings = files.map(_.toAbsolutePath.show)
      _ <- strings.toList.traverse_(Console[F].putStrLn)
    } yield ()

  def minus(left: Path, right: Path): F[Set[Path]] =
    for {
      leftWithSum <- dirToHashedFiles(left)
      rightWithSum <- dirToHashedFiles(right)
      sumToFile =
        leftWithSum
          .map { case FileAndHash(file, hash) => (hash, file) }
          .toMap
          .view
          .toMap
      result =
        leftWithSum
          .map(_.hash)
          .toSet
          .diff(rightWithSum.map(_.hash).toSet)
          .map(s => sumToFile(s))
    } yield result

  def dirToHashedFiles(file: Path): F[Vector[FileAndHash]] =
    Fs[F]
      .listRecursive(file, core.filePredicate)
      .flatMap(paths =>
        paths.traverseWithProgress(file => FileAndHash.fromFile[F](file))
      )
}
