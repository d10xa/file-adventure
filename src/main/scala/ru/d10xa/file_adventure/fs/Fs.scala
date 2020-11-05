package ru.d10xa.file_adventure.fs

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.attribute.BasicFileAttributes
import java.util.function.BiPredicate

import cats.Applicative

import scala.jdk.StreamConverters._
import cats.effect.Sync
import cats.implicits._
import ru.d10xa.file_adventure.implicits._

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
  def requireExists(paths: Path*): F[Unit]
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
            .toScala(Vector)
        )

      override def size(file: Path): F[Long] = Sync[F].delay(Files.size(file))

      override def exists(p: Path): F[Boolean] = Sync[F].delay(Files.exists(p))

      private def requireExistsOne(p: Path): F[Unit] =
        exists(p).ifM(
          Applicative[F].unit,
          Sync[F].raiseError(
            new IllegalArgumentException(s"path does not exist: ${p.show}")
          )
        )

      override def requireExists(paths: Path*): F[Unit] =
        paths.toList.traverse_(requireExistsOne)

      override def isRegularFile(p: Path): F[Boolean] =
        Sync[F].delay(Files.isRegularFile(p))

      override def linesVector(p: Path): F[Vector[String]] =
        Sync[F].delay(Files.lines(p).toScala(Vector))

    })
}
