package ru.d10xa.file_adventure

import better.files.File
import ru.d10xa.file_adventure.core.FileAndHash

trait FileEntryRepositoryAlgebra[F[_]] {
  def save(f: File): F[Int]
  def saveWithHash(fh: FileAndHash): F[Int]
}
