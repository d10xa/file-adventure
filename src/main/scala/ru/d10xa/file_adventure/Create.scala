package ru.d10xa.file_adventure

import better.files._

class Create(dir: File) {

  val FILESUM_CONSTANT_NAME = ".sha256.sfv"

  val calculateSums: List[File] => List[SumFileRow] = _.map(SumFileRow.fromFile)
  val sumsAsStrings: List[SumFileRow] => List[String] = _.map(_.asFileString)

  def run(): Unit = {

    def createShaFiles(parent: File, files: List[File]): Unit = {
      val content = sumsAsStrings.compose(calculateSums)(files).mkString("\n")
      File(parent, FILESUM_CONSTANT_NAME).write(content)
    }

    val x = dir.list(core.filePredicate).toList.groupBy(_.parent)
    x.foreach {
      case (parent: File, files: List[File]) => createShaFiles(parent, files)
    }
  }

}

final case class SumFileRow(regularFile: File, sum: String) {
  val asFileString: String = s"$sum *${regularFile.name}"
}

object SumFileRow {
  def fromFile(f: File): SumFileRow = SumFileRow(f, f.sha256.toLowerCase)
}
