package ru.d10xa.file_adventure

import better.files._
import cats.effect.Sync
import ru.d10xa.file_adventure.core.FileAndHash
import cats.implicits._
import ru.d10xa.file_adventure.fs.Checksum

import scala.util.chaining._

class Minus[F[_]: Sync: Checksum: Console] {

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

  val listFiles: File => Vector[File] = _.list(core.filePredicate).toVector

  def dirToHashedFiles(file: File): F[Vector[FileAndHash]] =
    listFiles(file).pipe(core.filesToHashesWithProgressBar[F])
}
