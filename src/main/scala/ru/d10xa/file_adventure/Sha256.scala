package ru.d10xa.file_adventure

import better.files._
import cats._
import cats.implicits._
import ru.d10xa.file_adventure.Main.sortHashes
import ru.d10xa.file_adventure.Main.sortedHashesToSingleHash
import ru.d10xa.file_adventure.core.FileAndHash

class Sha256(dir: File) {
  def run(): Unit =
    println(Sha256.recursiveHash(dir))
}

object Sha256 {

  private val hashOnly: FileAndHash => String = _.hash

  val filesToSingleHash: Vector[File] => String =
    sortedHashesToSingleHash
      .compose(sortHashes)
      .compose(Functor[Vector].lift(hashOnly))
      .compose(core.filesToHashesWithProgressBar.rmap(_.toVector))

  val recursiveHash: File => String =
    f => filesToSingleHash(f.list(core.filePredicate).toList.toVector)

}
