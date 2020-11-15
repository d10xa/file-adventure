package ru.d10xa.file_adventure.fs

import java.nio.file.Path

import cats.effect.Blocker
import cats.effect.ContextShift
import cats.effect.Sync
import fs2.Stream
import ru.d10xa.file_adventure.fs.PathStreamService.InnerSfv
import ru.d10xa.file_adventure.fs.PathStreamService.PlainFile
import ru.d10xa.file_adventure.fs.PathStreamService.ToCheck
import cats.implicits._
import ru.d10xa.file_adventure.fs.PathStreamService.OuterSfv
import ru.d10xa.file_adventure.implicits._

import scala.annotation.tailrec

class PathStreamService[F[_]: Sync: ContextShift: Fs] {

  /**
    * Given path /a/b/c. Method [[outerWalk]] will check
    * existence of following files:
    *
    *   /a/b/${sfvFileName}
    *   /a/${sfvFileName}
    *   /${sfvFileName}
    */
  def outerWalk(
    path: Path,
    sfvFileName: String
  ): Stream[F, OuterSfv] =
    Stream
      .unfold(path.toAbsolutePath)(p => Option(p.getParent).map(x => x -> x))
      .map(_.resolve(sfvFileName))
      .evalFilter(Fs[F].exists)
      .map(p => path.toAbsolutePath.relativize(p.toAbsolutePath))
      .map(p => OuterSfv(p))

  /**
    * Find all matched files recursively
    */
  def innerWalk(
    blocker: Blocker,
    path: Path,
    sfvFileName: String
  ): Stream[F, ToCheck] =
    fs2.io.file
      .walk[F](blocker, path)
      .evalFilter(Fs[F].isRegularFile)
      .map(path =>
        if (path.getFileName.show == sfvFileName) InnerSfv(path)
        else PlainFile(path)
      )

  /**
    * Join [[outerWalk]] and [[innerWalk]]
    */
  def inOutWalk(
    blocker: Blocker,
    path: Path,
    sfvFileName: String
  ): Stream[F, ToCheck] =
    outerWalk(path, sfvFileName) ++ innerWalk(
      blocker,
      path,
      sfvFileName
    )
}

object PathStreamService {
  sealed trait ToCheck {
    def path: Path
  }

  object ToCheck {

    def divide(
      list: Vector[ToCheck]
    ): (Vector[InnerSfv], Vector[OuterSfv], Vector[PlainFile]) =
      divide(list, Vector.empty, Vector.empty, Vector.empty)

    @tailrec
    private def divide(
      list: Vector[ToCheck],
      inners: Vector[InnerSfv],
      outers: Vector[OuterSfv],
      plains: Vector[PlainFile]
    ): (Vector[InnerSfv], Vector[OuterSfv], Vector[PlainFile]) =
      list match {
        case head +: tail =>
          head match {
            case a: InnerSfv => divide(tail, inners.:+(a), outers, plains)
            case b: OuterSfv => divide(tail, inners, outers.:+(b), plains)
            case c: PlainFile => divide(tail, inners, outers, plains.:+(c))
          }
        case _ => (inners, outers, plains)
      }
  }

  final case class InnerSfv(path: Path) extends ToCheck
  final case class OuterSfv(path: Path) extends ToCheck
  final case class PlainFile(path: Path) extends ToCheck
}
