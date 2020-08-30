package ru.d10xa.file_adventure

import java.nio.file.Path

import better.files._
import cats.implicits._
import cats.Functor
import cats.Show
import ru.d10xa.file_adventure.core.FileAndHash
import ru.d10xa.file_adventure.core.Sha256Hash

class Create(dir: File, oneFile: Boolean) {

  implicit val showPath: Show[Path] = Show.fromToString

  val calculateSums: List[File] => List[FileAndHash] =
    Functor[List].lift(FileAndHash.fromFile)

  val fahAsString: Path => FileAndHash => String = path => {
    case FileAndHash(file, Sha256Hash(hash)) =>
      s"${hash.show} *${path.relativize(file.path).show}"
  }

  def run(): Unit = {

    def createShaFiles(parent: File, files: List[File]): Unit = {
      val content = calculateSums
        .andThen(Functor[List].lift(fahAsString(parent.path)))(files)
        .mkString("\n")
      File(parent, core.FILESUM_CONSTANT_NAME).write(content)
    }

    if (oneFile) {
      val x: List[File] = dir.list(core.filePredicate).toList
      createShaFiles(dir, x)
    } else {
      val x = dir.list(core.filePredicate).toList.groupBy(_.parent)
      x.foreach {
        case (parent: File, files: List[File]) =>
          createShaFiles(parent, files)
      }
    }
  }

}
