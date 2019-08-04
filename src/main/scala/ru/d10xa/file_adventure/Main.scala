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

  def main(args: Array[String]): Unit = {
    val files = File(args.head)
      .list(filePredicate).toList
    val progress = new ProgressBar("", files.map(_.size).sum)

    val hash: String = files
      .map { file =>
        val sha = file.sha256.toLowerCase
        val _ = progress.stepBy(file.size)
        sha
      }
      .sortBy(new BigInteger(_, 16)) match {
      case x :: Nil => x
      case xs => sha256Hex(xs.mkString(""))
    }
    val _ = progress.stop()

    println(hash)
  }

}
