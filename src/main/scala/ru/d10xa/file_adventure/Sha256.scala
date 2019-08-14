package ru.d10xa.file_adventure

import better.files.File
import ru.d10xa.file_adventure.Main.filesToHashesWithProgressBar
import ru.d10xa.file_adventure.Main.sortHashes
import ru.d10xa.file_adventure.Main.sortedHashesToSingleHash

class Sha256(dir: File) {
  def run(): Unit = {
    val hash = Sha256.filesToSingleHash(
      dir
        .list(Main.filePredicate)
        .toList
        .toVector)
    println(hash)
  }
}

object Sha256 {
  val filesToSingleHash: Vector[File] => String =
    sortedHashesToSingleHash
      .compose(sortHashes)
      .compose(filesToHashesWithProgressBar)
}
