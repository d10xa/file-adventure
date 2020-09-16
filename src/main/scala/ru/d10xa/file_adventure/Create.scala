package ru.d10xa.file_adventure

import java.nio.file.Path

import better.files._
import cats._
import cats.implicits._
import ru.d10xa.file_adventure.core.FileAndHash
import ru.d10xa.file_adventure.core.Sha256Hash
import ru.d10xa.file_adventure.fs.Checksum
import ru.d10xa.file_adventure.fs.FileWrite

class Create[F[_]: Monad: FileWrite: Checksum] {

  implicit val showPath: Show[Path] = Show.fromToString

  def calculateSums(files: List[File]): F[List[FileAndHash]] =
    files.traverse(file => FileAndHash.fromFile[F](file))

  val fahAsString: Path => FileAndHash => String = path => {
    case FileAndHash(file, Sha256Hash(hash)) =>
      s"${hash.show} *${path.relativize(file.path).show}"
  }

  def run(c: CreateCommand): F[Unit] = {

    def createShaFiles(parent: File, files: List[File]): F[Unit] = {
      val content = calculateSums(files)
        .map(Functor[List].lift(fahAsString(parent.path)))
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
      val x: List[File] = dir.list(core.filePredicate).toList
      createShaFiles(dir, x)
    } else {
      val x = dir.list(core.filePredicate).toList.groupBy(_.parent)
      x.toList.traverse_ {
        case (parent: File, files: List[File]) =>
          createShaFiles(parent, files)
      }
    }
  }

}
