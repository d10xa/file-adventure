package ru.d10xa.file_adventure

import java.math.BigInteger

import better.files.File
import better.files._
import cats.Show
import cats.implicits._
import me.tongfei.progressbar.ProgressBar
import org.apache.commons.codec.digest.DigestUtils.sha256Hex

object core {

  implicit val showSha256Hash: Show[Sha256Hash] =
    Show[Sha256Hash](_.value.toString)

  final case class Sha256Hash(value: String) extends AnyRef {
    def asBigInteger: BigInt = new BigInteger(value, 16)
  }

  object Sha256Hash {
    def fromString(str: String): Sha256Hash = Sha256Hash(sha256Hex(str))
    def fromFile(file: File): Sha256Hash = Sha256Hash(file.sha256.toLowerCase)
  }

  final case class FileAndHash(regularFile: File, hash: Sha256Hash) {
    val asFileString: String = s"${hash.show} *${regularFile.name}"
  }

  object FileAndHash {
    val fileToHash: File => Sha256Hash = f => Sha256Hash(f.sha256.toLowerCase)

    def fromFile(f: File): FileAndHash = FileAndHash(f, fileToHash(f))
    val fromLine: String => FileAndHash = _.split(" [*,\\s]").toList match {
      case sum :: file :: Nil => FileAndHash(File(file), Sha256Hash(sum))
      case xs => throw new IllegalArgumentException(xs.mkString("[", ",", "]"))
    }
  }

  def filePredicate(f: File): Boolean =
    Seq(
      !f.isHidden,
      !f.name.startsWith("."),
//      !f.name.endsWith(".sfv"),
      f.isRegularFile
    ).forall(identity)

  val filesToHashesWithProgressBar: Iterable[File] => Iterable[FileAndHash] =
    files => {
      new ProgressBar("", files.map(_.size).sum).autoClosed
        .map(bar =>
          files.map { file =>
            val _ = bar.setExtraMessage(file.name)
            val h = FileAndHash.fromFile(file)
            val _ = bar.stepBy(file.size)
            h
        })
        .get()
    }

}
