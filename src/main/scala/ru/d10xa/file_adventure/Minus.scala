package ru.d10xa.file_adventure

import better.files._
import me.tongfei.progressbar.ProgressBar

class Minus(left: File, right: File) {

  def run(): Unit = {
    val leftFiles = left
      .list(Main.filePredicate)
      .toList
    val rightFiles = right
      .list(Main.filePredicate)
      .toList

    def fileWithSum(file: File, bar: ProgressBar): (File, String) = {
      val _ = bar.setExtraMessage(file.name)
      val sum = file.sha256.toLowerCase
      val _ = bar.stepBy(file.size)
      (file, sum)
    }

    val leftWithSum: List[(File, String)] = (for {
      bar <- new ProgressBar("", leftFiles.map(_.size).sum).autoClosed
    } yield leftFiles.map(fileWithSum(_, bar))).get()

    val rightWithSum: List[(File, String)] = (for {
      bar <- new ProgressBar("", rightFiles.map(_.size).sum).autoClosed
    } yield rightFiles.map(fileWithSum(_, bar))).get()

    val sumToLeftName: Map[String, String] =
      leftWithSum
        .map(_.swap)
        .toMap
        .view
        .mapValues(_.toJava.getAbsolutePath)
        .toMap

    val resultFiles =
      leftWithSum
        .map(_._2)
        .diff(rightWithSum.map(_._2))
        .map(s => sumToLeftName(s))

    resultFiles.foreach(println(_))
  }
}
