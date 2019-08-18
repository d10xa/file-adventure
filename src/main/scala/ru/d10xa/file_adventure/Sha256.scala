package ru.d10xa.file_adventure

import better.files._
import cats._
import cats.implicits._
import me.tongfei.progressbar.ProgressBar
import ru.d10xa.file_adventure.Main.sortHashes
import ru.d10xa.file_adventure.Main.sortedHashesToSingleHash
import ru.d10xa.file_adventure.core.FileAndHash

class Sha256(dir: File) {
  def run(): Unit =
    println(Sha256.recursiveHash(dir))
}

object Sha256 {

  private val hashOnly: FileAndHash => String = _.sum

  val filesToHashesWithProgressBar: Vector[File] => Vector[FileAndHash] =
    files => {
      new ProgressBar("", files.map(_.size).sum).autoClosed
        .map(bar =>
          files.map { file =>
            val _ = bar.setExtraMessage(file.name)
            val hash = core.fileToHash(file)
            val _ = bar.stepBy(file.size)
            FileAndHash(file, hash)
        })
        .get()
    }

  val filesToSingleHash: Vector[File] => String =
    sortedHashesToSingleHash
      .compose(sortHashes)
      .compose(Functor[Vector].lift(hashOnly))
      .compose(filesToHashesWithProgressBar)

  val recursiveHash: File => String =
    f => filesToSingleHash(f.list(core.filePredicate).toList.toVector)

}
