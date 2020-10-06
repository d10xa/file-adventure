package ru.d10xa.file_adventure

import java.nio.file.Path

import cats.Applicative
import cats.ApplicativeError
import cats.Show
import cats.implicits._
import ru.d10xa.file_adventure.fs.PathSize
import ru.d10xa.file_adventure.progress.ProgressInfo
import ru.d10xa.file_adventure.progress.TraverseProgress.TraverseProgressImplicits

trait CatsInstances {
  implicit val catsShowForPath: Show[Path] = Show.fromToString
}

trait JavaNioImplicits extends CatsInstances {
  implicit class PathOpsF(
    private val f: Path
  ) {
    def parentF[F[_]: ApplicativeError[*[_], Throwable]]: F[Path] =
      Either
        .fromOption(
          Option(f.getParent),
          new IllegalArgumentException(
            s"parent folder not available: ${f.show}"
          )
        )
        .liftTo[F]
  }

  implicit class PathOps(private val f: Path) {
    def nameOrEmpty: String =
      Option(f.getFileName).map(_.show).getOrElse("")
    def absolutePathUnsafe: String =
      f.toAbsolutePath.show
  }

}

trait ProgressInstances extends JavaNioImplicits {
  implicit def fileToCheckProgressInfo[F[_]: PathSize: Applicative]
    : ProgressInfo[F, FileToCheck] =
    new ProgressInfo[F, FileToCheck] {
      override def step(a: FileToCheck): F[Long] =
        PathSize[F].size(a.file)
      override def message(a: FileToCheck): F[String] =
        a.file.nameOrEmpty.pure[F]
    }
  implicit def javaNioPathProgressInfo[F[_]: PathSize: Applicative]
    : ProgressInfo[F, Path] =
    new ProgressInfo[F, Path] {
      override def step(p: Path): F[Long] = PathSize[F].size(p)
      override def message(p: Path): F[String] = p.nameOrEmpty.pure[F]
    }
}

object implicits
    extends CatsInstances
    with JavaNioImplicits
    with ProgressInstances
    with TraverseProgressImplicits
