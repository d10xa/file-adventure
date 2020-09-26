package ru.d10xa.file_adventure

import better.files._
import ru.d10xa.file_adventure.core.FileAndHash
import cats._
import cats.effect.Bracket
import cats.implicits._
import ru.d10xa.file_adventure.Progress.ProgressBuilder
import ru.d10xa.file_adventure.fs.Checksum
import ru.d10xa.file_adventure.fs.Fs

class Minus[F[_]: Monad: Bracket[*[_], Throwable]: Checksum: Console](
  fs: Fs[F],
  progressBuilder: ProgressBuilder[F]
) {

  def run(c: MinusCommand): F[Unit] =
    for {
      files <- minus(File(c.left), File(c.right))
      strings = files.map(_.toJava.getAbsolutePath)
      _ <- strings.toList.traverse_(Console[F].putStrLn)
    } yield ()

  def minus(left: File, right: File): F[Set[File]] =
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

  def dirToHashedFiles(file: File): F[Vector[FileAndHash]] =
    fs.listRecursive(file.path, core.filePredicate)
      .flatMap(
        core.filesToHashesWithProgressBar[F](progressBuilder, _)
      )
}
