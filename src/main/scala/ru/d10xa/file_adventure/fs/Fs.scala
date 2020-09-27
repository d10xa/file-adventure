package ru.d10xa.file_adventure.fs

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.attribute.BasicFileAttributes
import java.util.function.BiPredicate
import scala.jdk.CollectionConverters._

import cats.effect.Sync

trait Fs[F[_]] {
  def listRecursive(dir: Path, predicate: Path => Boolean): F[Vector[Path]]
}
object Fs {
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
    })
}
