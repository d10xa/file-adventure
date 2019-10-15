package ru.d10xa.file_adventure

import better.files._
import ru.d10xa.file_adventure.core.FileAndHash

class Create(dir: File) {

  val calculateSums: List[File] => List[FileAndHash] =
    _.map(FileAndHash.fromFile)

  val sumsAsStrings: List[FileAndHash] => List[String] = _.map(_.asFileString)

  def run(): Unit = {

    def createShaFiles(parent: File, files: List[File]): Unit = {
      val content = calculateSums.andThen(sumsAsStrings)(files).mkString("\n")
      File(parent, core.FILESUM_CONSTANT_NAME).write(content)
    }

    val x = dir.list(core.filePredicate).toList.groupBy(_.parent)
    x.foreach {
      case (parent: File, files: List[File]) => createShaFiles(parent, files)
    }
  }

}
