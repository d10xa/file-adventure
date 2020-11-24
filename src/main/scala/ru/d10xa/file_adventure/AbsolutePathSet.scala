package ru.d10xa.file_adventure

import cats.Foldable
import cats.syntax.all._
import ru.d10xa.file_adventure.fs.PathStreamService.PlainFile
import ru.d10xa.file_adventure.implicits._

class AbsolutePathSet private (absNormPathSet: Set[String]) {
  def contains(f: PlainFile): Boolean =
    absNormPathSet.contains(f.path.normalize().toAbsolutePath.show)
}

object AbsolutePathSet {
  def apply[T[_]: Foldable](items: T[PlainFile]): AbsolutePathSet =
    new AbsolutePathSet(
      items.foldLeft(Set.empty[String])((set, plainFile) =>
        set + plainFile.path.normalize().toAbsolutePath.show
      )
    )
}
