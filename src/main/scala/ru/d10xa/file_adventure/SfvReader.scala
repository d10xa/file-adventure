package ru.d10xa.file_adventure

import java.nio.file.Path

import cats.Applicative
import cats.MonadError
import ru.d10xa.file_adventure.fs.Fs
import ru.d10xa.file_adventure.implicits._
import cats.implicits._

trait SfvReader[F[_]] {
  def readRecursiveSfvFiles(
    path: Path,
    sfvFileName: String
  ): F[Vector[FileToCheck]]
}

object SfvReader {
  def make[F[_]: Applicative: Fs](implicit
    ME: MonadError[F, Throwable]
  ): SfvReader[F] =
    new SfvReader[F] {
      def listRecursiveSumFiles(
        path: Path,
        sfvFileName: String
      ): F[Vector[Path]] =
        Fs[F].listRecursive(path, p => p.nameOrEmpty === sfvFileName)

      def filterRegularFiles(paths: Vector[Path]): F[Vector[Path]] =
        paths.filterA(p => Fs[F].isRegularFile(p))

      def readSumFiles(paths: Vector[Path]): F[Vector[FileToCheck]] =
        paths.flatTraverse(FileToCheck.readFromSumFile[F])

      override def readRecursiveSfvFiles(
        path: Path,
        sfvFileName: String
      ): F[Vector[FileToCheck]] =
        listRecursiveSumFiles(path, sfvFileName)
          .flatMap(filterRegularFiles)
          .flatMap(readSumFiles)
    }

}
