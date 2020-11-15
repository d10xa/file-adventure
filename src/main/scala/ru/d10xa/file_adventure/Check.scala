package ru.d10xa.file_adventure

import java.nio.file.Path

import better.files.File
import cats._
import cats.effect.Blocker
import cats.effect.Bracket
import cats.effect.Concurrent
import cats.effect.ContextShift
import cats.implicits._
import ru.d10xa.file_adventure.core.FileAndHash
import ru.d10xa.file_adventure.core.Sha256Hash
import ru.d10xa.file_adventure.fs.Checksum
import ru.d10xa.file_adventure.core._
import ru.d10xa.file_adventure.fs.Fs
import ru.d10xa.file_adventure.fs.PathStreamService
import ru.d10xa.file_adventure.fs.PathStreamService.InnerSfv
import ru.d10xa.file_adventure.fs.PathStreamService.OuterSfv
import ru.d10xa.file_adventure.fs.PathStreamService.PlainFile
import ru.d10xa.file_adventure.fs.PathStreamService.ToCheck
import ru.d10xa.file_adventure.implicits._
import ru.d10xa.file_adventure.progress.TraverseProgress

class Check[F[_]: Bracket[
  *[_],
  Throwable
]: Concurrent: ContextShift: Fs: TraverseProgress: Checksum: Log](
  pathStreamService: PathStreamService[F]
) {

  val sfvFileName: String = FILESUM_CONSTANT_NAME

  def check(fileToCheck: SfvItem): F[CheckedFile] =
    for {
      _ <- Log[F].debug(s"start check (${fileToCheck.file.show})")
      res <- fileToCheck.check[F]()
      _ <- Log[F].debug(s"valid: ${res.valid.show} (${fileToCheck.file.show})")
    } yield res

  def plainFilesToAbsNamesSet(files: Vector[PlainFile]): Set[String] =
    files.map(_.path.normalize().toAbsolutePath.show).toSet

  def checkDir(dir: Path): F[Vector[CheckedFile]] =
    for {
      toCheck <- Blocker[F].use(blocker =>
        pathStreamService.inOutWalk(blocker, dir, sfvFileName).compile.toVector
      )
      (inners, outers, plain) = ToCheck.divide(toCheck)
      sfvItemsOuters <-
        outers
          .traverse {
            case OuterSfv(path) => SfvItem.readFromSumFile(dir.resolve(path))
          }
          .map(_.flatten.filter(item => dir.isParentFor(item.file)))
      sfvItemsInners <-
        inners
          .traverse { case InnerSfv(path) => SfvItem.readFromSumFile(path) }
          .map(_.flatten)
      sfvItems = sfvItemsOuters ++ sfvItemsInners
      checked <- sfvItems.traverseWithProgress(check)
      absPathsSet = plainFilesToAbsNamesSet(plain)
      untracked =
        plain
          .filter {
            case PlainFile(path) =>
              !absPathsSet.contains(path.normalize().toAbsolutePath.show)
          }
          .map { case PlainFile(path) => UntrackedFile(path) }
    } yield checked ++ untracked

  def trackedPredicate(absRealPathsFromSfv: Set[String])(path: Path): Boolean =
    absRealPathsFromSfv.contains(path.normalize().toAbsolutePath.show)

  def checkDirs(dirs: Vector[Path]): F[Vector[CheckedFile]] =
    dirs
      .foldLeftM(Vector.empty[CheckedFile]) {
        case (acc, f) => checkDir(f).map(acc ++ _)
      }

  def run(c: CheckCommand): F[Unit] =
    checkDirs(Vector(c.dir))
      .flatTap(list => Log[F].debug(s"Total checked: ${list.size.show}"))
      .map(list =>
        list
          .filter {
            case e: ExistentCheckedFile => !e.valid
            case _ => true
          }
          .map(_.show)
      )
      .flatMap(list =>
        Log[F].debug(s"Total invalid: ${list.size.show}") >>
          list.traverse_(item => Log[F].info(item))
      )

}

final case class DirsToCheck(dirs: Vector[File]) {
  require(
    dirs.forall(_.isDirectory),
    "DirsToCheck must be initialized only with directories"
  )
}

// https://en.wikipedia.org/wiki/Simple_file_verification
final case class SfvItem(file: Path, checksum: Sha256Hash) {
  def check[F[_]: Monad: Fs: Checksum](): F[CheckedFile] = {
    val missingCase =
      FileSystemMissingFile(file, checksum)
        .pure[F]
        .widen[CheckedFile]
    val okCase =
      Checksum[F]
        .sha256(file)
        .map(sha => ExistentCheckedFile(file, checksum, sha))
        .widen[CheckedFile]
    Fs[F].isRegularFile(file).ifM[CheckedFile](okCase, missingCase)
  }
}

object SfvItem {

  def fileAndHashToFileToCheck(
    parent: Path,
    fileAndHash: FileAndHash
  ): SfvItem =
    SfvItem(parent.resolve(fileAndHash.regularFile), fileAndHash.hash)

  def linesToFtc[F[_]: MonadError[*[_], Throwable]](
    file: Path,
    lines: Vector[String]
  ): F[Vector[SfvItem]] =
    lines
      .traverse[F, SfvItem](line =>
        file
          .parentF[F]
          .flatMap(parent =>
            FileAndHash
              .fromLine[F](parent, line)
              .map(fileAndHash => fileAndHashToFileToCheck(parent, fileAndHash))
          )
      )

  def readFromSumFile[F[_]: Fs: MonadError[*[_], Throwable]](
    file: Path
  ): F[Vector[SfvItem]] =
    Fs[F].linesVector(file).flatMap(linesToFtc[F](file, _))

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
      case UntrackedFile(file) =>
        s"UNTRACKED FILE ${file.show}"
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
  file: Path
) extends CheckedFile {
  override def valid: Boolean = false
}
