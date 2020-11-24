package ru.d10xa.file_adventure

import java.math.BigInteger
import java.nio.file.Files
import java.nio.file.Path

import cats._
import cats.implicits._
import org.apache.commons.codec.digest.DigestUtils.sha256Hex
import ru.d10xa.file_adventure.fs.Checksum
import ru.d10xa.file_adventure.implicits._

object core {

  val FILESUM_CONSTANT_NAME = ".sha256.sfv"

  final case class Sha256Sum(value: String) extends AnyRef {
    def asBigInteger: BigInt = new BigInteger(value, 16)
  }

  object Sha256Sum {
    implicit val eqSha256Hash: Eq[Sha256Sum] = Eq.fromUniversalEquals
    implicit val showSha256Hash: Show[Sha256Sum] =
      Show[Sha256Sum](_.value)
    def fromString(str: String): Sha256Sum = Sha256Sum(sha256Hex(str))
  }

  final case class FileAndHash(regularFile: Path, hash: Sha256Sum) {
    val asFileString: String = s"${hash.show} *${regularFile.show}"
  }

  object FileAndHash {

    def fromFile[F[_]: Functor: Checksum](f: Path): F[FileAndHash] =
      Checksum[F]
        .sha256(f)
        .map(hash => FileAndHash(f, hash))

    def fromLine[F[_]: ApplicativeError[*[_], Throwable]](
      parent: Path,
      line: String
    ): F[FileAndHash] =
      line.split(" [*,\\s]", 2).toList match {
        case sum :: file :: Nil =>
          FileAndHash(parent.relativize(parent.resolve(file)), Sha256Sum(sum))
            .pure[F]
        case xs =>
          new IllegalArgumentException(xs.mkString("[", ",", "]"))
            .raiseError[F, FileAndHash]
      }
  }

  def filePredicate(f: Path): Boolean =
    Seq(
      !Files.isHidden(f),
      Option(f.getFileName).exists(!_.show.startsWith(".")),
      Files.isRegularFile(f),
      Files.exists(f) // files listed but not exists in folder '.@__thumb/'
    ).forall(identity)

}
