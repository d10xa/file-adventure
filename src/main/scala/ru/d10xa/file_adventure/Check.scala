package ru.d10xa.file_adventure

import java.nio.file.Path

import better.files._
import cats._
import cats.effect.Sync
import cats.implicits._
import ru.d10xa.file_adventure.core.FileAndHash
import ru.d10xa.file_adventure.core.Sha256Hash
import core._

class Check[F[_]: Sync: Console] {

  def checkDir(dir: File): F[List[CheckedFile]] = {
    val regularFiles: F[List[File]] =
      Sync[F].delay(dir.list.toList).map(_.filter(core.filePredicate))

    val file = File(dir, FILESUM_CONSTANT_NAME)

    val filesToCheck: F[List[FileToCheck]] =
      file.fileExistsF
        .ifM[List[FileToCheck]](
          FileToCheck.readFromSumFile(file),
          List.empty[FileToCheck].pure[F]
        )

    val checkedFiles: F[List[CheckedFile]] =
      filesToCheck.flatMap(_.traverse(_.check()))

    val fileNamesFromSumFile: F[Set[String]] =
      filesToCheck.map(_.map(_.file.name).toSet)

    def toUntracked(files: List[File]): F[List[UntrackedFile]] =
      files.traverse(file =>
        Sha256Hash.fromFile(file).map(hash => UntrackedFile(file, hash))
      )

    val untrackedFiles: F[List[UntrackedFile]] = regularFiles
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

  final def checkDirs(
    dirsToCheck: DirsToCheck
  ): F[List[CheckedFile]] = {
    type ResultType = (List[CheckedFile], List[File])
    val dirs = dirsToCheck.dirs
    val tailRecResult = Monad[F].tailRecM((List.empty[CheckedFile], dirs)) {
      case (checkedFiles, dirs) =>
        dirs match {
          case xs if xs.isEmpty =>
            val res = (checkedFiles, dirs)
            res.asRight[ResultType].pure[F]
          case x :: xs =>
            checkDir(x).map(xf =>
              (checkedFiles ++ xf, x.list.filter(_.isDirectory).toList ++ xs)
                .asLeft[ResultType]
            )
        }
    }
    tailRecResult.map(_._1)
  }

  def run(c: CheckCommand): F[Unit] =
    checkDirs(DirsToCheck(File(c.dir) :: Nil))
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

final case class DirsToCheck(dirs: List[File]) {
  require(
    dirs.forall(_.isDirectory),
    "DirsToCheck must be initialized only with directories"
  )
}

final case class FileToCheck(file: File, expectedHash: Sha256Hash) {
  implicit val catsShowForPath: Show[Path] = Show.fromToString[Path]

  require(
    file.isRegularFile,
    s"FileToCheck must be initialized only with files. (${file.path.show})"
  )
  def check[F[_]: Sync](): F[CheckedFile] =
    if (!file.exists || !file.isRegularFile)
      FileSystemMissingFile(file, expectedHash).pure[F].widen
    else
      Sha256Hash
        .fromFile[F](file)
        .map(sha => ExistentCheckedFile(file, expectedHash, sha))
}

object FileToCheck {

  def fileAndHashToFileToCheck(fileAndHash: FileAndHash): FileToCheck =
    FileToCheck(fileAndHash.regularFile, fileAndHash.hash)

  def readFromSumFile[F[_]: Sync](file: File): F[List[FileToCheck]] =
    file.lines.toList
      .traverse(line => FileAndHash.fromLine(file.parent, line))
      .map(list => Functor[List].lift(fileAndHashToFileToCheck _)(list))
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
