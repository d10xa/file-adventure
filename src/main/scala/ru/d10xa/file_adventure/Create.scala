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

class Create[F[_]: Monad: FileWrite: Checksum: Bracket[*[_], Throwable]](
  progressBuilder: ProgressBuilder[F]
) {

  implicit val showPath: Show[Path] = Show.fromToString

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

    if (c.oneFile) {
      val x: Vector[File] = dir.list(core.filePredicate).toVector
      createShaFiles(dir, x)
    } else {
      val x = dir.list(core.filePredicate).toVector.groupBy(_.parent)
      x.toVector.traverse_ {
        case (parent: File, files: Vector[File]) =>
          createShaFiles(parent, files)
      }
    }
  }

}
