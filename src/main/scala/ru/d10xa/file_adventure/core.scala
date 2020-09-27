package ru.d10xa.file_adventure

import java.math.BigInteger
import java.nio.file.Files
import java.nio.file.Path

import better.files.File
import cats._
import cats.effect.Bracket
import cats.effect.Sync
import cats.implicits._
import org.apache.commons.codec.digest.DigestUtils.sha256Hex
import ru.d10xa.file_adventure.Progress.ProgressBuilder
import ru.d10xa.file_adventure.Progress.InitParams
import ru.d10xa.file_adventure.fs.Checksum
import ru.d10xa.file_adventure.implicits._

object core {

  val FILESUM_CONSTANT_NAME = ".sha256.sfv"

  // TODO rename to Sha256Sum
  final case class Sha256Hash(value: String) extends AnyRef {
    def asBigInteger: BigInt = new BigInteger(value, 16)
  }

  object Sha256Hash {
    implicit val eqSha256Hash: Eq[Sha256Hash] = Eq.fromUniversalEquals
    implicit val showSha256Hash: Show[Sha256Hash] =
      Show[Sha256Hash](_.value)
    def fromString(str: String): Sha256Hash = Sha256Hash(sha256Hex(str))
  }

  final case class FileAndHash(regularFile: Path, hash: Sha256Hash) {
    val asFileString: String = s"${hash.show} *${regularFile.show}"
  }

  object FileAndHash {

    def fromFile[F[_]: Functor: Checksum](f: Path): F[FileAndHash] =
      Checksum[F]
        .sha256(f)
        .map(hash => FileAndHash(f, hash))

    def fromLine[F[_]: Sync](parent: Path, line: String): F[FileAndHash] =
      line.split(" [*,\\s]").toList match {
        case sum :: file :: Nil =>
          FileAndHash(parent.resolve(file), Sha256Hash(sum)).pure[F]
        case xs =>
          Sync[F].raiseError(
            new IllegalArgumentException(xs.mkString("[", ",", "]"))
          )
      }
  }

  def filePredicate(f: Path): Boolean =
    Seq(
      !Files.isHidden(f),
      Option(f.getFileName).exists(!_.show.startsWith(".")),
      Files.isRegularFile(f),
      Files.exists(f) // files listed but not exists in folder '.@__thumb/'
    ).forall(identity)

  def filesToHashesWithProgressBar[
    F[_]: Monad: Checksum: Bracket[*[_], Throwable]
  ](
    progressBuilder: ProgressBuilder[F],
    files: Vector[Path]
  ): F[Vector[FileAndHash]] =
    progressBuilder
      .build(
        InitParams(
          "",
          files.foldLeft(0L) { case (acc, path) => acc + File(path).size }
        )
      )
      .use(progress =>
        files.traverse { file =>
          for {
            _ <- progress.setExtraMessage(file.nameOrEmpty)
            h <- FileAndHash.fromFile[F](file)
            _ <- progress.stepBy(Files.size(file))
          } yield h
        }
      )

}
