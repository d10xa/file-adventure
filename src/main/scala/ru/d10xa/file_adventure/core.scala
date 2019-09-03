package ru.d10xa.file_adventure

import better.files.File
import better.files._
import me.tongfei.progressbar.ProgressBar

object core {

  final case class FileAndHash(regularFile: File, hash: String) {
    val asFileString: String = s"$hash *${regularFile.name}"
  }

  object FileAndHash {
    def fromFile(f: File): FileAndHash = FileAndHash(f, f.sha256.toLowerCase)
    val fromLine: String => FileAndHash = _.split(" [*,\\s]").toList match {
      case sum :: file :: Nil => FileAndHash(File(file), sum)
      case xs => throw new IllegalArgumentException(xs.mkString("[", ",", "]"))
    }
  }

  def filePredicate(f: File): Boolean =
    Seq(
      !f.isHidden,
      !f.name.startsWith("."),
      !f.name.endsWith(".sfv"),
      f.isRegularFile
    ).forall(identity)

  val fileToHash: File => String = _.sha256.toLowerCase

  val filesToHashesWithProgressBar: Iterable[File] => Iterable[FileAndHash] =
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

}
