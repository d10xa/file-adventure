package ru.d10xa.file_adventure

import better.files.File

object core {

  final case class FileAndHash(regularFile: File, sum: String) {
    val asFileString: String = s"$sum *${regularFile.name}"
  }

  object FileAndHash {
    def fromFile(f: File): FileAndHash = FileAndHash(f, f.sha256.toLowerCase)
  }

  def filePredicate(f: File): Boolean =
    Seq(
      !f.isHidden,
      !f.name.startsWith("."),
      !f.name.endsWith(".sfv"),
      f.isRegularFile
    ).forall(identity)

  val fileToHash: File => String = _.sha256.toLowerCase

}
