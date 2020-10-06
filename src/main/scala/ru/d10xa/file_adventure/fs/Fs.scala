package ru.d10xa.file_adventure.fs

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.attribute.BasicFileAttributes
import java.util.function.BiPredicate
import scala.jdk.CollectionConverters._

import cats.effect.Sync

trait Fs[F[_]]
    extends PathSize[F]
    with PathList[F]
    with PathExist[F]
    with PathLines[F]

trait PathSize[F[_]] {
  def size(file: Path): F[Long]
}
object PathSize {
  def apply[F[_]](implicit instance: PathSize[F]): PathSize[F] = instance
}
trait PathExist[F[_]] {
  def exists(p: Path): F[Boolean]
  def isRegularFile(p: Path): F[Boolean]
}
trait PathList[F[_]] {
  def listRecursive(dir: Path, predicate: Path => Boolean): F[Vector[Path]]
}
trait PathLines[F[_]] {
  def linesVector(p: Path): F[Vector[String]]
}
object Fs {
  def apply[F[_]](implicit instance: Fs[F]): Fs[F] = instance
  def make[F[_]: Sync]: F[Fs[F]] =
    Sync[F].delay(new Fs[F] {
      override def listRecursive(
        dir: Path,
        predicate: Path => Boolean
      ): F[Vector[Path]] =
        Sync[F].delay(
          Files
            .find(
              dir,
              Int.MaxValue,
              new BiPredicate[Path, BasicFileAttributes] {
                override def test(t: Path, u: BasicFileAttributes): Boolean =
                  predicate(t)
              }
            )
            .iterator()
            .asScala
            .toVector
        )

      override def size(file: Path): F[Long] = Sync[F].delay(Files.size(file))

      override def exists(p: Path): F[Boolean] = Sync[F].delay(Files.exists(p))

      override def isRegularFile(p: Path): F[Boolean] =
        Sync[F].delay(Files.isRegularFile(p))

      override def linesVector(p: Path): F[Vector[String]] = {
        import scala.jdk.CollectionConverters._
        Sync[F].delay(Files.lines(p).iterator().asScala.toVector)
      }
    })
}
