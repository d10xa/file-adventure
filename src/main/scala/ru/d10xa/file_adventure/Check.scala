package ru.d10xa.file_adventure

import better.files._
import cats.implicits._
import ru.d10xa.file_adventure.core.FileAndHash
import ru.d10xa.file_adventure.core.Sha256Hash
import core._

import scala.annotation.tailrec

class Check(dir: File) {

  def checkDir(dir: File): List[CheckedFile] = {
    val regularFiles: List[File] = dir.list.toList.filter(core.filePredicate)
    val filesToCheckOpt: Option[List[FileToCheck]] =
      File(dir, FILESUM_CONSTANT_NAME).fileExistsOption
        .map(FileToCheck.readFromSumFile)
    val filesToCheck: List[FileToCheck] =
      filesToCheckOpt.getOrElse(List.empty[FileToCheck])
    val checkedFiles: List[CheckedFile] = filesToCheck.map(_.check())
    val fileNamesFromSumFile: Set[String] = filesToCheck.map(_.file.name).toSet
    val untrackedFiles: List[UntrackedFile] = regularFiles
      .filter(file => !fileNamesFromSumFile.contains(file.name))
      .map(file => UntrackedFile(file, Sha256Hash.fromFile(file)))
    checkedFiles ++ untrackedFiles
  }

  @tailrec
  final def checkDirs(
    checkedFiles: List[CheckedFile],
    dirs: DirsToCheck
  ): List[CheckedFile] =
    dirs.dirs match {
      case xs if xs.isEmpty => checkedFiles
      case x :: xs =>
        checkDirs(
          checkedFiles ++ checkDir(x),
          DirsToCheck(x.list.filter(_.isDirectory).toList ++ xs)
        )
    }

  def run(): Unit =
    checkDirs(List.empty[CheckedFile], DirsToCheck(dir :: Nil))
      .filter {
        case e: ExistentCheckedFile => !e.valid
        case _ => true
      }
      .map(_.show)
      .foreach(println(_))
}

final case class DirsToCheck(dirs: List[File]) {
  require(
    dirs.forall(_.isDirectory),
    "DirsToCheck must be initialized only with directories")
}

final case class FileToCheck(file: File, expectedHash: Sha256Hash) {
  require(file.isRegularFile, "FileToCheck must be initialized only with files")
  def check(): CheckedFile =
    if (!file.exists || !file.isRegularFile) {
      FileSystemMissingFile(file, expectedHash)
    } else {
      ExistentCheckedFile(file, expectedHash, Sha256Hash.fromFile(file))
    }
}

object FileToCheck {
  def readFromSumFile(file: File): List[FileToCheck] =
    file.lines.toList
      .map(FileAndHash.fromLine(file.parent)(_))
      .map { case FileAndHash(f, h) => FileToCheck(f, h) }
}

sealed trait CheckedFile {
  def valid: Boolean
}

final case class ExistentCheckedFile(
  file: File,
  expectedHash: Sha256Hash,
  actualHash: Sha256Hash
) extends CheckedFile {
  override def valid: Boolean = expectedHash === actualHash
}

final case class FileSystemMissingFile(
  file: File,
  expectedHash: Sha256Hash
) extends CheckedFile {
  override def valid: Boolean = false
}

final case class UntrackedFile(
  file: File,
  actualHash: Sha256Hash
) extends CheckedFile {
  override def valid: Boolean = false
}
