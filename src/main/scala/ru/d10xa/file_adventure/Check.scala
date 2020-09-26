package ru.d10xa.file_adventure

import better.files.File
import cats._
import cats.effect.Sync
import cats.implicits._
import ru.d10xa.file_adventure.core.FileAndHash
import ru.d10xa.file_adventure.core.Sha256Hash
import ru.d10xa.file_adventure.fs.Checksum
import core._
import ru.d10xa.file_adventure.fs.Fs

class Check[F[_]: Sync: Checksum: Console](fs: Fs[F]) {

  def checkDir(dir: File): F[Vector[CheckedFile]] = {
    val regularFiles: F[Vector[File]] =
      fs.listRecursive(dir.path, core.filePredicate)

    val file = File(dir, FILESUM_CONSTANT_NAME)

    val filesToCheck: F[Vector[FileToCheck]] =
      file.fileExistsF
        .ifM[Vector[FileToCheck]](
          FileToCheck.readFromSumFile(file),
          Vector.empty[FileToCheck].pure[F]
        )

    val checkedFiles: F[Vector[CheckedFile]] =
      filesToCheck.flatMap(_.traverse(_.check()))

    val fileNamesFromSumFile: F[Set[String]] =
      filesToCheck.map(_.map(_.file.name).toSet)

    def toUntracked(files: Vector[File]): F[Vector[UntrackedFile]] =
      files.traverse(file =>
        Checksum[F].sha256(file.path).map(hash => UntrackedFile(file, hash))
      )

    val untrackedFiles: F[Vector[UntrackedFile]] = regularFiles
      .flatMap { files =>
        files
          .filterA(file =>
            fileNamesFromSumFile.map(set => !set.contains(file.name))
          )
      }
      .flatMap(toUntracked)

    for {
      a <- checkedFiles
      b <- untrackedFiles
    } yield a ++ b
  }

  def checkDirs(dirs: Vector[File]): F[Vector[CheckedFile]] =
    dirs
      .foldLeftM(Vector.empty[CheckedFile]) {
        case (acc, f) => checkDir(f).map(acc ++ _)
      }

  def run(c: CheckCommand): F[Unit] =
    checkDirs(Vector(File(c.dir)))
      .map(list =>
        list
          .filter {
            case e: ExistentCheckedFile => !e.valid
            case _ => true
          }
          .map(_.show)
      )
      .flatMap(list => list.traverse_(item => Console[F].putStrLn(item)))

}

final case class DirsToCheck(dirs: Vector[File]) {
  require(
    dirs.forall(_.isDirectory),
    "DirsToCheck must be initialized only with directories"
  )
}

final case class FileToCheck(file: File, expectedHash: Sha256Hash) {
  import ru.d10xa.file_adventure.implicits._
  require(
    file.isRegularFile,
    s"FileToCheck must be initialized only with files. (${file.path.show})"
  )
  def check[F[_]: Sync: Checksum](): F[CheckedFile] =
    if (!file.exists || !file.isRegularFile)
      FileSystemMissingFile(file, expectedHash).pure[F].widen
    else
      Checksum[F]
        .sha256(file.path)
        .map(sha => ExistentCheckedFile(file, expectedHash, sha))
}

object FileToCheck {

  def fileAndHashToFileToCheck(fileAndHash: FileAndHash): FileToCheck =
    FileToCheck(fileAndHash.regularFile, fileAndHash.hash)

  def readFromSumFile[F[_]: Sync](file: File): F[Vector[FileToCheck]] =
    file.lines.toVector
      .traverse(line => FileAndHash.fromLine(file.parent, line))
      .map(list => Functor[Vector].lift(fileAndHashToFileToCheck _)(list))
}

sealed trait CheckedFile {
  def valid: Boolean
}

object CheckedFile {
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
