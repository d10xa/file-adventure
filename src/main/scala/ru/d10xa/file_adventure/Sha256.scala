package ru.d10xa.file_adventure

import better.files._
import cats._
import cats.implicits._
import ru.d10xa.file_adventure.Main.sortHashes
import ru.d10xa.file_adventure.Main.sortedHashesToSingleHash
import ru.d10xa.file_adventure.core.FileAndHash
import ru.d10xa.file_adventure.core.Sha256Hash

class Sha256(dir: File) {
  def run(): Unit =
    println(Sha256.recursiveHash(dir).show)
}

object Sha256 {

  private val hashOnly: FileAndHash => Sha256Hash = fileAndHash =>
    fileAndHash.hash

  val filesToSingleHash: Vector[File] => Sha256Hash =
    sortedHashesToSingleHash
      .compose(sortHashes)
      .compose(Functor[Vector].lift(hashOnly))
      .compose(core.filesToHashesWithProgressBar.rmap(_.toVector))

  val recursiveHash: File => Sha256Hash =
    f => filesToSingleHash(f.list(core.filePredicate).toList.toVector)

}
