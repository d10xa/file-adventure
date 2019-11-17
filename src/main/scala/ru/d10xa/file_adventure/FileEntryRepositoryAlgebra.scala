package ru.d10xa.file_adventure

import ru.d10xa.file_adventure.core.FileAndHash

trait FileEntryRepositoryAlgebra[F[_]] {
  def saveWithHash(fh: FileAndHash): F[Int]
}
