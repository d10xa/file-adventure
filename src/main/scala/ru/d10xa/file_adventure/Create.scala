package ru.d10xa.file_adventure

import java.nio.file.Path
import java.nio.file.Paths

import cats._
import cats.effect.Bracket
import cats.effect.Sync
import cats.implicits._
import ru.d10xa.file_adventure.core.FileAndHash
import ru.d10xa.file_adventure.core.Sha256Hash
import ru.d10xa.file_adventure.fs.Checksum
import ru.d10xa.file_adventure.fs.FileWrite
import ru.d10xa.file_adventure.fs.Fs
import ru.d10xa.file_adventure.implicits._
import ru.d10xa.file_adventure.progress.Progress.ProgressBuilder
import ru.d10xa.file_adventure.progress.TraverseProgress._

class Create[F[
  _
]: Fs: Sync: ProgressBuilder: Monad: FileWrite: Checksum: Bracket[*[
  _
], Throwable]] {

  def calculateSums(files: Vector[Path]): F[Vector[FileAndHash]] =
    files.traverseWithProgress(f => FileAndHash.fromFile[F](f))

  private def traverseCreate(x: Vector[(Path, Vector[Path])]): F[Unit] =
    x.traverse_ {
      case (parent: Path, files: Vector[Path]) =>
        createShaFiles(parent, files)
    }

  private def createShaFiles(parent: Path, files: Vector[Path]): F[Unit] = {
    val content = calculateSums(files)
      .map(v =>
        v.map {
          case FileAndHash(file, Sha256Hash(hash)) =>
            s"${hash.show} *${parent.relativize(file).show}"
        }
      )
      .map(_.mkString("\n"))

    content.flatMap(c =>
      FileWrite[F].writeString(
        parent.resolve(core.FILESUM_CONSTANT_NAME),
        c
      )
    )
  }

  def run(c: CreateCommand): F[Unit] = {
    val dir = Paths.get(c.dir)

    if (c.oneFile)
      Fs[F]
        .listRecursive(dir, core.filePredicate)
        .flatMap(createShaFiles(dir, _))
    else
      Fs[F]
        .listRecursive(dir, core.filePredicate)
        .map(_.groupBy(_.getParent).toVector)
        .flatMap(traverseCreate)
  }

}
