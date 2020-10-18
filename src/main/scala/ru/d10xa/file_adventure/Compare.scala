package ru.d10xa.file_adventure

import java.nio.file.Path

import cats.Eq
import cats.Monad
import io.circe.Encoder
import ru.d10xa.file_adventure.Compare.CompareResult
import ru.d10xa.file_adventure.Compare.ComparisonSide
import ru.d10xa.file_adventure.fs.Fs
import ru.d10xa.file_adventure.core.FILESUM_CONSTANT_NAME
import ru.d10xa.file_adventure.implicits._
import io.circe.syntax._
import cats.implicits._

import scala.annotation.nowarn

class Compare[F[_]: Fs: Monad: Console](
  sfvReader: SfvReader[F],
  sfvService: SfvService[F]
) {

  val sfvFileName: String = FILESUM_CONSTANT_NAME

  implicit val eqByPathAndHash: Eq[FileToCheck] =
    Eq.instance {
      case (FileToCheck(fl, sl), FileToCheck(fr, sr)) =>
        fl.nameOrEmpty === fr.nameOrEmpty && sl.value === sr.value
    }

  def compareSides(
    currentSideParent: Path,
    otherSideParent: Path,
    currentSide: Vector[FileToCheck],
    otherSide: Vector[FileToCheck]
  ): ComparisonSide =
    ComparisonSide(
      duplicatesBySum =
        sfvService.duplicatesBySum(currentSide, otherSide).map(_.file),
      duplicatesByPathAndSum = sfvService
        .duplicatesByPathAndSum(
          currentSideParent,
          otherSideParent,
          currentSide,
          otherSide
        )
        .map(_.file)
    )

  def run(c: CompareCommand): F[Unit] =
    for {
      _ <- Fs[F].requireExists(c.left, c.right)
      leftSfvs <- sfvReader.readRecursiveSfvFiles(c.left, sfvFileName)
      rightSfvs <- sfvReader.readRecursiveSfvFiles(c.right, sfvFileName)
      compareResult = CompareResult(
        left = compareSides(c.left, c.right, leftSfvs, rightSfvs),
        right = compareSides(c.right, c.left, rightSfvs, leftSfvs)
      )
      _ <- Console[F].putStrLn(compareResult.asJson.spaces2)
    } yield ()

}

object Compare {
  final case class ComparisonSide(
    duplicatesBySum: Vector[Path],
    duplicatesByPathAndSum: Vector[Path]
  )

  object ComparisonSide {
    @SuppressWarnings(Array("org.wartremover.warts.ToString"))
    implicit val circeJavaNioFilePathEncoder: Encoder[Path] =
      Encoder.encodeString.contramap[Path](_.toString)
    @nowarn(
      "msg=Block\\sresult\\swas\\sadapted\\svia\\simplicit\\sconversion.+"
    )
    implicit val circeEncoderComparisonSide: Encoder[ComparisonSide] =
      io.circe.generic.semiauto.deriveEncoder[ComparisonSide]
  }

  final case class CompareResult(left: ComparisonSide, right: ComparisonSide)

  object CompareResult {
    @nowarn(
      "msg=Block\\sresult\\swas\\sadapted\\svia\\simplicit\\sconversion.+"
    )
    implicit val circeEncoderCompareResult: Encoder[CompareResult] =
      io.circe.generic.semiauto.deriveEncoder[CompareResult]
  }

}
