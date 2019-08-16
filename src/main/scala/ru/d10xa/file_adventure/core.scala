package ru.d10xa.file_adventure

import better.files.File

object core {
  def filePredicate(f: File): Boolean =
    Seq(
      !f.isHidden,
      !f.name.startsWith("."),
      !f.name.endsWith(".sfv"),
      f.isRegularFile
    ).forall(identity)

  val fileToHash: File => String = _.sha256.toLowerCase

}
