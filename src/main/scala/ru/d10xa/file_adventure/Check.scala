package ru.d10xa.file_adventure

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths

import better.files.File
import cats._
import cats.effect.Sync
import cats.implicits._
import ru.d10xa.file_adventure.core.FileAndHash
import ru.d10xa.file_adventure.core.Sha256Hash
import ru.d10xa.file_adventure.fs.Checksum
import ru.d10xa.file_adventure.core._
import ru.d10xa.file_adventure.fs.Fs
import ru.d10xa.file_adventure.implicits._
import ru.d10xa.file_adventure.progress.TraverseProgress._

class Check[F[_]: Sync: Checksum: Console](fs: Fs[F]) {

  def checkDir(dir: Path): F[Vector[CheckedFile]] = {
    val regularFiles: F[Vector[Path]] =
      fs.listRecursive(dir, core.filePredicate)

    val file = dir.resolve(FILESUM_CONSTANT_NAME)

    val filesToCheck: F[Vector[FileToCheck]] =
      file.fileExistsF
        .ifM[Vector[FileToCheck]](
          FileToCheck.readFromSumFile(file),
          Vector.empty[FileToCheck].pure[F]
        )

    val checkedFiles: F[Vector[CheckedFile]] =
      filesToCheck.flatMap(_.traverseWithProgress(_.check()))

    val fileNamesFromSumFile: F[Set[String]] =
      filesToCheck.map(_.map(_.file.nameOrEmpty).toSet)

    def toUntracked(files: Vector[Path]): F[Vector[UntrackedFile]] =
      files.traverse(file =>
        Checksum[F].sha256(file).map(hash => UntrackedFile(file, hash))
      )

    val untrackedFiles: F[Vector[UntrackedFile]] = regularFiles
      .flatMap { files =>
        files
          .filterA(file =>
            fileNamesFromSumFile.map(set =>
              !set.contains(Option(file.getFileName.show).getOrElse(""))
            )
          )
      }
      .flatMap(toUntracked)

    for {
      a <- checkedFiles
      b <- untrackedFiles
    } yield a ++ b
  }

  def checkDirs(dirs: Vector[Path]): F[Vector[CheckedFile]] =
    dirs
      .foldLeftM(Vector.empty[CheckedFile]) {
        case (acc, f) => checkDir(f).map(acc ++ _)
      }

  def run(c: CheckCommand): F[Unit] =
    checkDirs(Vector(Paths.get(c.dir)))
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

final case class FileToCheck(file: Path, expectedHash: Sha256Hash) {
  import ru.d10xa.file_adventure.implicits._
  require(
    Files.isRegularFile(file),
    s"FileToCheck must be initialized only with files. (${file.show})"
  )
  def check[F[_]: Sync: Checksum](): F[CheckedFile] = {
    val isMissing = for {
      e <- file.existsF
      r <- file.isRegularFileF
    } yield !e || !r
    val missingCase =
      Sync[F]
        .delay(FileSystemMissingFile(file, expectedHash))
        .widen[CheckedFile]
    val okCase =
      Checksum[F]
        .sha256(file)
        .map(sha => ExistentCheckedFile(file, expectedHash, sha))
        .widen[CheckedFile]
    isMissing.ifM[CheckedFile](missingCase, okCase)
  }
}

object FileToCheck {

  def fileAndHashToFileToCheck(
    parent: Path,
    fileAndHash: FileAndHash
  ): FileToCheck =
    FileToCheck(parent.resolve(fileAndHash.regularFile), fileAndHash.hash)

  def linesToFtc[F[_]: Sync](
    file: Path,
    lines: Vector[String]
  ): F[Vector[FileToCheck]] =
    lines
      .traverse(line =>
        file.parentF.flatMap(parent =>
          FileAndHash
            .fromLine(parent, line)
            .map(fileAndHash => fileAndHashToFileToCheck(parent, fileAndHash))
        )
      )

  def readFromSumFile[F[_]: Sync](file: Path): F[Vector[FileToCheck]] =
    Sync[F].delay(File(file).lines.toVector).flatMap(linesToFtc(file, _))

}

sealed trait CheckedFile {
  def valid: Boolean
}

object CheckedFile {
  implicit val showCheckedFile: Show[CheckedFile] =
    Show.show {
      case e @ ExistentCheckedFile(file, expectedHash, _) if e.valid =>
        s"OK ${file.show} ${expectedHash.show}"
      case ExistentCheckedFile(file, expectedHash, actualHash) =>
        s"FAIL ${file.show} ${expectedHash.show} != ${actualHash.show}"
      case FileSystemMissingFile(file, expectedHash) =>
        s"FILE NOT FOUND ${file.show}, ${expectedHash.show}"
      case UntrackedFile(file, actualHash) =>
        s"UNTRACKED FILE ${file.show}, ${actualHash.show}"
    }
}

final case class ExistentCheckedFile(
  file: Path,
  expectedHash: Sha256Hash,
  actualHash: Sha256Hash
) extends CheckedFile {
  override def valid: Boolean = expectedHash === actualHash
}

final case class FileSystemMissingFile(
  file: Path,
  expectedHash: Sha256Hash
) extends CheckedFile {
  override def valid: Boolean = false
}

final case class UntrackedFile(
  file: Path,
  actualHash: Sha256Hash
) extends CheckedFile {
  override def valid: Boolean = false
}
