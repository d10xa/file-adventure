package ru.d10xa.file_adventure

import java.nio.file.Path

import better.files._
import cats._
import cats.effect.Bracket
import cats.implicits._
import ru.d10xa.file_adventure.Progress.ProgressBuilder
import ru.d10xa.file_adventure.core.FileAndHash
import ru.d10xa.file_adventure.core.Sha256Hash
import ru.d10xa.file_adventure.fs.Checksum
import ru.d10xa.file_adventure.fs.FileWrite
import ru.d10xa.file_adventure.fs.Fs
import ru.d10xa.file_adventure.implicits._

class Create[F[_]: Monad: FileWrite: Checksum: Bracket[*[_], Throwable]](
  fs: Fs[F],
  progressBuilder: ProgressBuilder[F]
) {

  def calculateSums(files: Vector[File]): F[Vector[FileAndHash]] =
    core.filesToHashesWithProgressBar(progressBuilder, files)

  val fahAsString: Path => FileAndHash => String = path => {
    case FileAndHash(file, Sha256Hash(hash)) =>
      s"${hash.show} *${path.relativize(file.path).show}"
  }

  def run(c: CreateCommand): F[Unit] = {

    def createShaFiles(parent: File, files: Vector[File]): F[Unit] = {
      val content = calculateSums(files)
        .map(Functor[Vector].lift(fahAsString(parent.path)))
        .map(_.mkString("\n"))
      content.flatMap(c =>
        FileWrite[F].writeString(
          parent.path.resolve(core.FILESUM_CONSTANT_NAME),
          c
        )
      )
    }
    val dir = File(c.dir)

    def traverseCreate(x: Vector[(File, Vector[File])]): F[Unit] =
      x.traverse_ {
        case (parent: File, files: Vector[File]) =>
          createShaFiles(parent, files)
      }

    if (c.oneFile)
      fs.listRecursive(dir.path, core.filePredicate)
        .flatMap(createShaFiles(dir, _))
    else
      fs.listRecursive(dir.path, core.filePredicate)
        .map(_.groupBy(_.parent).toVector)
        .flatMap(traverseCreate)
  }

}
