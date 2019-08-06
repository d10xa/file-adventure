package ru.d10xa.file_adventure

import java.math.BigInteger

import better.files._
import me.tongfei.progressbar.ProgressBar
import org.apache.commons.codec.digest.DigestUtils.sha256Hex


object Main {

  def filePredicate(f: File): Boolean = {
    Seq(
      !f.isHidden,
      !f.name.startsWith("."),
      !f.name.endsWith(".sfv"),
      f.isRegularFile
    ).forall(identity)
  }

  def fileToHash(file: File, bar: ProgressBar): String = {
    val _ = bar.setExtraMessage(file.name)
    val sum = file.sha256.toLowerCase
    val _ = bar.stepBy(file.size)
    sum
  }

  val filesToHashes: Vector[File] => Vector[String] = files => (
    for {
      bar <- new ProgressBar("", files.map(_.size).sum).autoClosed
    } yield files.map(fileToHash(_, bar))
    ).get()

  val sortHashes: Vector[String] => Vector[String] = _.sortBy(new BigInteger(_, 16))

  val toSingleHash: Vector[String] => String = {
    case Vector(x: String) => x
    case xs => sha256Hex(xs.mkString(""))
  }

  def main(args: Array[String]): Unit = {
    val files = File(args.head)
      .list(filePredicate)
      .toList

    val hash: String = (toSingleHash compose sortHashes compose filesToHashes) (files.toVector)

    println(hash)
  }

}
