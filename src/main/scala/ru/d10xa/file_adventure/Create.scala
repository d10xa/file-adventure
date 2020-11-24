package ru.d10xa.file_adventure

import java.nio.file.Path

import cats._
import cats.implicits._
import ru.d10xa.file_adventure.core.FileAndHash
import ru.d10xa.file_adventure.core.Sha256Sum
import ru.d10xa.file_adventure.fs.Checksum
import ru.d10xa.file_adventure.fs.FileWrite
import ru.d10xa.file_adventure.fs.Fs
import ru.d10xa.file_adventure.implicits._
import ru.d10xa.file_adventure.progress.TraverseProgress

class Create[F[_]: Fs: TraverseProgress: Monad: FileWrite: Checksum] {

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
          case FileAndHash(file, Sha256Sum(hash)) =>
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

  def handleOneFileTrue(dir: Path): F[Unit] =
    Fs[F]
      .listRecursive(dir, core.filePredicate)
      .flatMap(createShaFiles(dir, _))

  def handleOneFileFalse(dir: Path): F[Unit] =
    Fs[F]
      .listRecursive(dir, core.filePredicate)
      .map(_.groupBy(_.getParent).toVector)
      .flatMap(traverseCreate)

  def run(c: CreateCommand): F[Unit] = {
    val handler = if (c.oneFile) handleOneFileTrue _ else handleOneFileFalse _
    c.dirs.traverse_(handler)
  }

}
