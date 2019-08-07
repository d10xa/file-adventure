package ru.d10xa.file_adventure

import java.math.BigInteger

import better.files._
import me.tongfei.progressbar.ProgressBar
import org.apache.commons.codec.digest.DigestUtils.sha256Hex

object Main {

  def filePredicate(f: File): Boolean =
    Seq(
      !f.isHidden,
      !f.name.startsWith("."),
      !f.name.endsWith(".sfv"),
      f.isRegularFile
    ).forall(identity)

  val fileToHash: File => String = _.sha256.toLowerCase

  val filesToHashWithProgressBar: Vector[File] => Vector[String] = files => {
    new ProgressBar("", files.map(_.size).sum).autoClosed
      .map(bar =>
        files.map { file =>
          val _ = bar.setExtraMessage(file.name)
          val hash = fileToHash(file)
          val _ = bar.stepBy(file.size)
          hash
      })
      .get()
  }

  val sortHashes: Vector[String] => Vector[String] =
    _.sortBy(new BigInteger(_, 16))

  val sortedHashesToSingleHash: Vector[String] => String = {
    case Vector(x: String) => x
    case xs => sha256Hex(xs.mkString(""))
  }

  val filesToSingleHash: Vector[File] => String =
    sortedHashesToSingleHash
      .compose(sortHashes)
      .compose(filesToHashWithProgressBar)

  def main(args: Array[String]): Unit = {
    val files = File(args.head)
      .list(filePredicate)
      .toList

    val hash: String = filesToSingleHash(files.toVector)

    println(hash)
  }

}
