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
import ru.d10xa.file_adventure.SfvItem
import ru.d10xa.file_adventure.fs.PathStreamService.OuterSfv
import ru.d10xa.file_adventure.implicits._

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

  /**
    * Returns two separate streams:
    *  1) sfv items (entries from checksum files)
    *  2) plain files
    */
  def sfvItemsAndPlainFilesSeparate(
    blocker: Blocker,
    dir: Path,
    sfvFileName: String
  ): fs2.Stream[F, (fs2.Stream[F, SfvItem], fs2.Stream[F, PlainFile])] =
    for {
      (innerStream, plainStream) <- innerWalkSeparate(blocker, dir, sfvFileName)
      outerStream = outerWalk(dir, sfvFileName)
      innerSfvItemsStream =
        innerStream
          .evalMap {
            case InnerSfv(path) =>
              SfvItem.readFromSumFile(path)
          }
          .flatMap(fs2.Stream.emits)
      outerSfvItemsStream =
        outerStream
          .evalMap {
            case OuterSfv(path) =>
              SfvItem.readFromSumFile(dir.resolve(path))
          }
          .flatMap(fs2.Stream.emits)
      sfvItemsInScope =
        fs2
          .Stream(innerSfvItemsStream, outerSfvItemsStream)
          .parJoinUnbounded
          .filter(item => dir.isParentFor(item.file))
    } yield (sfvItemsInScope, plainStream)
}

object PathStreamService {
  sealed trait ToCheck {
    def path: Path
  }

  final case class InnerSfv(path: Path) extends ToCheck
  final case class OuterSfv(path: Path) extends ToCheck
  final case class PlainFile(path: Path) extends ToCheck
}
