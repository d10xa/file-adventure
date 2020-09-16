package ru.d10xa.file_adventure

import java.math.BigInteger

import better.files.File
import cats._
import cats.effect.Resource
import cats.effect.Sync
import cats.implicits._
import me.tongfei.progressbar.ProgressBar
import org.apache.commons.codec.digest.DigestUtils.sha256Hex
import ru.d10xa.file_adventure.fs.Checksum

object core {

  val FILESUM_CONSTANT_NAME = ".sha256.sfv"

  implicit class FileOps[F[_]: Sync](f: File) {
    def fileExistsF: F[Boolean] =
      for {
        exists <- Sync[F].delay(f.exists)
        regularFile <- Sync[F].delay(f.isRegularFile)
      } yield exists && regularFile
  }

  implicit val showSha256Hash: Show[Sha256Hash] =
    Show[Sha256Hash](_.value)

  implicit val showCheckedFile: Show[CheckedFile] =
    Show.show {
      case e @ ExistentCheckedFile(file, expectedHash, _) if e.valid =>
        s"OK ${file.toJava.getAbsolutePath} ${expectedHash.show}"
      case ExistentCheckedFile(file, expectedHash, actualHash) =>
        s"FAIL ${file.toJava.getAbsolutePath} ${expectedHash.show} != ${actualHash.show}"
      case FileSystemMissingFile(file, expectedHash) =>
        s"FILE NOT FOUND ${file.toJava.getAbsolutePath}, ${expectedHash.show}"
      case UntrackedFile(file, actualHash) =>
        s"UNTRACKED FILE ${file.toJava.getAbsolutePath}, ${actualHash.show}"
    }

  // TODO rename to Sha256Sum
  final case class Sha256Hash(value: String) extends AnyRef {
    def asBigInteger: BigInt = new BigInteger(value, 16)
  }

  object Sha256Hash {
    implicit val eqSha256Hash: Eq[Sha256Hash] = Eq.fromUniversalEquals
    def fromString(str: String): Sha256Hash = Sha256Hash(sha256Hex(str))
    def fromFile[F[_]: Sync](file: File): F[Sha256Hash] =
      Sync[F]
        .delay(file.sha256.toLowerCase)
        .map(sha => Sha256Hash(sha))
  }

  final case class FileAndHash(regularFile: File, hash: Sha256Hash) {
    val asFileString: String = s"${hash.show} *${regularFile.name}"
  }

  object FileAndHash {

    def fromFile[F[_]: Functor: Checksum](f: File): F[FileAndHash] =
      Checksum[F]
        .sha256(f.path)
        .map(hash => FileAndHash(f, hash))

    def fromLine[F[_]: Sync](parent: File, line: String): F[FileAndHash] =
      line.split(" [*,\\s]").toList match {
        case sum :: file :: Nil =>
          FileAndHash(File(parent, file), Sha256Hash(sum)).pure[F]
        case xs =>
          Sync[F].raiseError(
            new IllegalArgumentException(xs.mkString("[", ",", "]"))
          )
      }
  }

  def filePredicate(f: File): Boolean =
    Seq(
      !f.isHidden,
      !f.name.startsWith("."),
      f.isRegularFile
    ).forall(identity)

  def filesToHashesWithProgressBar[F[_]: Sync: Checksum](
    files: Vector[File]
  ): F[Vector[FileAndHash]] =
    Resource
      .fromAutoCloseable(
        Sync[F].delay(new ProgressBar("", files.map(_.size).sum))
      )
      .use(bar =>
        files.traverse { file =>
          for {
            _ <- Sync[F].delay(bar.setExtraMessage(file.name))
            h <- FileAndHash.fromFile[F](file)
            _ <- Sync[F].delay(bar.stepBy(file.size))
          } yield h
        }
      )

}
