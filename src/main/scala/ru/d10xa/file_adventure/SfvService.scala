package ru.d10xa.file_adventure

import java.nio.file.Path

import cats.implicits._
import ru.d10xa.file_adventure.SfvService.SfvCompareResult
import ru.d10xa.file_adventure.implicits._

trait SfvService[F[_]] {
  def compareBySum(
    currentSide: Vector[SfvItem],
    otherSide: Vector[SfvItem]
  ): SfvCompareResult
  def compareByPathAndSum(
    currentSideParent: Path,
    otherSideParent: Path,
    currentSide: Vector[SfvItem],
    otherSide: Vector[SfvItem]
  ): SfvCompareResult
}

object SfvService {

  final case class SfvCompareResult(
    duplicates: Vector[SfvItem],
    unique: Vector[SfvItem]
  )

  def make[F[_]]: SfvService[F] =
    new SfvService[F] {
      override def compareBySum(
        currentSide: Vector[SfvItem],
        otherSide: Vector[SfvItem]
      ): SfvCompareResult = {
        val otherSideHashes = otherSide.map(_.checksum.value).toSet
        def isDuplicate(sfv: SfvItem): Boolean =
          otherSideHashes.contains(sfv.checksum.value)
        val (dupl, uniq) = currentSide.partition(isDuplicate)
        SfvCompareResult(
          duplicates = dupl,
          unique = uniq
        )
      }

      override def compareByPathAndSum(
        currentSideParent: Path,
        otherSideParent: Path,
        currentSide: Vector[SfvItem],
        otherSide: Vector[SfvItem]
      ): SfvCompareResult = {
        val otherSideSet: Set[(String, String)] =
          otherSide
            .map(sfv =>
              otherSideParent
                .relativize(sfv.file)
                .show -> sfv.checksum.value
            )
            .toSet
        def isDuplicate(sfv: SfvItem): Boolean =
          otherSideSet.contains(
            (
              currentSideParent.relativize(sfv.file).show,
              sfv.checksum.value
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
