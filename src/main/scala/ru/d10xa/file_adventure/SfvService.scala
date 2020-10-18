package ru.d10xa.file_adventure

import java.nio.file.Path

import cats.implicits._
import ru.d10xa.file_adventure.implicits._

trait SfvService[F[_]] {
  def duplicatesBySum(
    currentSide: Vector[FileToCheck],
    otherSide: Vector[FileToCheck]
  ): Vector[FileToCheck]
  def duplicatesByPathAndSum(
    currentSideParent: Path,
    otherSideParent: Path,
    currentSide: Vector[FileToCheck],
    otherSide: Vector[FileToCheck]
  ): Vector[FileToCheck]
}

object SfvService {
  def make[F[_]]: SfvService[F] =
    new SfvService[F] {
      override def duplicatesBySum(
        currentSide: Vector[FileToCheck],
        otherSide: Vector[FileToCheck]
      ): Vector[FileToCheck] = {
        val otherSideHashes = otherSide.map(_.expectedHash.value).toSet
        def isDuplicate(sfv: FileToCheck): Boolean =
          otherSideHashes.contains(sfv.expectedHash.value)
        currentSide.filter(isDuplicate)
      }

      override def duplicatesByPathAndSum(
        currentSideParent: Path,
        otherSideParent: Path,
        currentSide: Vector[FileToCheck],
        otherSide: Vector[FileToCheck]
      ): Vector[FileToCheck] = {
        val otherSideSet: Set[(String, String)] =
          otherSide
            .map(sfv =>
              otherSideParent
                .relativize(sfv.file)
                .show -> sfv.expectedHash.value
            )
            .toSet
        def isDuplicate(sfv: FileToCheck): Boolean =
          otherSideSet.contains(
            (
              currentSideParent.relativize(sfv.file).show,
              sfv.expectedHash.value
            )
          )
        currentSide.filter(isDuplicate)
      }
    }
}
