package ru.d10xa.file_adventure

import java.nio.file.Files
import java.nio.file.Path

import cats.Show
import cats.effect.Sync
import cats.implicits._

trait CatsInstances {
  implicit val catsShowForPath: Show[Path] = Show.fromToString
}

trait JavaNioImplicits extends CatsInstances {
  implicit class PathOpsF[F[_]: Sync](private val f: Path) {
    def fileExistsF: F[Boolean] =
      for {
        exists <- Sync[F].delay(Files.exists(f))
        regularFile <- Sync[F].delay(Files.isRegularFile(f))
      } yield exists && regularFile
    def parentF: F[Path] =
      Either
        .fromOption(
          Option(f.getParent),
          new IllegalArgumentException(
            s"parent folder not available: ${f.show}"
          )
        )
        .liftTo[F]
    def existsF: F[Boolean] = Sync[F].delay(Files.exists(f))
    def isRegularFileF: F[Boolean] = Sync[F].delay(Files.isRegularFile(f))
  }

  implicit class PathOps(private val f: Path) {
    def nameOrEmpty: String =
      Option(f.getFileName).map(_.show).getOrElse("")
    def absolutePathUnsafe: String =
      f.toAbsolutePath.show
  }

}

object implicits extends CatsInstances with JavaNioImplicits
