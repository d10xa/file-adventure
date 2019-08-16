package ru.d10xa.file_adventure

import better.files.File
import cats._
import cats.implicits._
import ru.d10xa.file_adventure.Main.filesToHashesWithProgressBar
import ru.d10xa.file_adventure.Main.sortHashes
import ru.d10xa.file_adventure.Main.sortedHashesToSingleHash
import ru.d10xa.file_adventure.core.FileAndHash

class Sha256(dir: File) {
  def run(): Unit = {
    val hash = Sha256.filesToSingleHash(
      dir
        .list(core.filePredicate)
        .toList
        .toVector)
    println(hash)
  }
}

object Sha256 {

  private val hashOnly: FileAndHash => String = _.sum

  val filesToSingleHash: Vector[File] => String =
    sortedHashesToSingleHash
      .compose(sortHashes)
      .compose(Functor[Vector].lift(hashOnly))
      .compose(filesToHashesWithProgressBar)

}
