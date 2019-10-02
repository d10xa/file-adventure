package ru.d10xa.file_adventure

import better.files._
import ru.d10xa.file_adventure.core.FileAndHash
import ru.d10xa.file_adventure.core.Sha256Hash

class Minus(left: File, right: File) {

  import Minus._

  def run(): Unit =
    minus(left, right)
      .map(_.toJava.getAbsolutePath)
      .foreach(println(_))
}

object Minus {
  def minus(left: File, right: File): Set[File] = {
    val leftWithSum: Iterable[FileAndHash] = dirToHashedFiles(left)
    val rightWithSum: Iterable[FileAndHash] = dirToHashedFiles(right)

    val sumToFile: Map[Sha256Hash, File] =
      leftWithSum
        .map { case FileAndHash(file, hash) => (hash, file) }
        .toMap
        .view
        .toMap

    leftWithSum
      .map(_.hash)
      .toSet
      .diff(rightWithSum.map(_.hash).toSet)
      .map(s => sumToFile(s))
  }
  val listFiles: File => List[File] = _.list(core.filePredicate).toList
  val dirToHashedFiles: File => Iterable[FileAndHash] =
    listFiles.andThen(core.filesToHashesWithProgressBar)
}
