package ru.d10xa.file_adventure.fs

import java.nio.file.Path

import cats.effect.Blocker
import cats.effect.Concurrent
import cats.effect.ContextShift
import fs2.Stream
import ru.d10xa.file_adventure.fs.PathStreamService.InnerSfv
import ru.d10xa.file_adventure.fs.PathStreamService.PlainFile
import ru.d10xa.file_adventure.fs.PathStreamService.ToCheck
import cats.implicits._
import fs2.concurrent.NoneTerminatedQueue
import fs2.concurrent.Queue
import ru.d10xa.file_adventure.fs.PathStreamService.OuterSfv
import ru.d10xa.file_adventure.implicits._

import scala.annotation.tailrec

class PathStreamService[F[_]: Concurrent: ContextShift: Fs] {

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
    for {
      (s1, s2) <- innerWalkSeparate(blocker, path, sfvFileName)
      s3 <- Stream(s1.widen[ToCheck], s2.widen[ToCheck]).parJoinUnbounded
    } yield s3

  def innerWalkSeparate(
    blocker: Blocker,
    path: Path,
    sfvFileName: String
  ): Stream[F, (Stream[F, InnerSfv], Stream[F, PlainFile])] = {

    def withQueue(
      qInner: NoneTerminatedQueue[F, InnerSfv],
      qPlain: NoneTerminatedQueue[F, PlainFile]
    ): Stream[F, (Stream[F, InnerSfv], Stream[F, PlainFile])] = {
      val enqueueSome = fs2.io.file
        .walk[F](blocker, path)
        .evalFilter(Fs[F].isRegularFile)
        .evalTap(path =>
          if (path.getFileName.show == sfvFileName)
            qInner.enqueue1(Some(InnerSfv(path)))
          else qPlain.enqueue1(Some(PlainFile(path)))
        )
        .drain
      val enqueueNone =
        Stream.eval(qInner.enqueue1(None)) ++ Stream.eval(qPlain.enqueue1(None))
      Stream
        .emit((qInner.dequeue, qPlain.dequeue))
        .concurrently(enqueueSome ++ enqueueNone)
    }

    for {
      qInner <- Stream.eval(Queue.noneTerminated[F, InnerSfv])
      qPlain <- Stream.eval(Queue.noneTerminated[F, PlainFile])
      s <- withQueue(qInner, qPlain)
    } yield s
  }

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
