package ru.d10xa.file_adventure

import java.nio.file.Path

import cats.implicits._
import ru.d10xa.file_adventure.SfvService.SfvCompareResult
import ru.d10xa.file_adventure.implicits._

trait SfvService[F[_]] {
  def compareBySum(
    currentSide: Vector[FileToCheck],
    otherSide: Vector[FileToCheck]
  ): SfvCompareResult
  def compareByPathAndSum(
    currentSideParent: Path,
    otherSideParent: Path,
    currentSide: Vector[FileToCheck],
    otherSide: Vector[FileToCheck]
  ): SfvCompareResult
}

object SfvService {

  final case class SfvCompareResult(
    duplicates: Vector[FileToCheck],
    unique: Vector[FileToCheck]
  )

  def make[F[_]]: SfvService[F] =
    new SfvService[F] {
      override def compareBySum(
        currentSide: Vector[FileToCheck],
        otherSide: Vector[FileToCheck]
      ): SfvCompareResult = {
        val otherSideHashes = otherSide.map(_.expectedHash.value).toSet
        def isDuplicate(sfv: FileToCheck): Boolean =
          otherSideHashes.contains(sfv.expectedHash.value)
        val (dupl, uniq) = currentSide.partition(isDuplicate)
        SfvCompareResult(
          duplicates = dupl,
          unique = uniq
        )
      }

      override def compareByPathAndSum(
        currentSideParent: Path,
        otherSideParent: Path,
        currentSide: Vector[FileToCheck],
        otherSide: Vector[FileToCheck]
      ): SfvCompareResult = {
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
        val (dupl, uniq) = currentSide.partition(isDuplicate)
        SfvCompareResult(
          duplicates = dupl,
          unique = uniq
        )
      }
    }
}
