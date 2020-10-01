package ru.d10xa.file_adventure.progress

import cats.implicits._
import cats.Traverse
import cats.effect.Bracket
import ru.d10xa.file_adventure.progress.Progress.InitParams
import ru.d10xa.file_adventure.progress.Progress.ProgressBuilder

trait TraverseProgress[F[_]] {
  def traverseWithProgress[A, B, T[_]: Traverse](
    items: T[A],
    f: A => F[B]
  )(implicit progressInfo: ProgressInfo[F, A]): F[T[B]]
}

trait ProgressInfo[F[_], A] {
  def step(a: A): F[Long]
  def message(a: A): F[String]
}

object TraverseProgress {

  implicit final class TraverseProgressOps[
    T[_]: Traverse,
    F[_]: ProgressBuilder: Bracket[*[_], Throwable],
    A,
    B
  ](private val a: T[A])(implicit progressInfo: ProgressInfo[F, A]) {
    def traverseWithProgress(f: A => F[B]): F[T[B]] =
      make[F](ProgressBuilder[F])
        .traverseWithProgress(a, f)
  }

  def make[F[_]: Bracket[*[_], Throwable]](
    progressBuilder: ProgressBuilder[F]
  ): TraverseProgress[F] =
    new TraverseProgress[F] {
      override def traverseWithProgress[A, B, T[_]: Traverse](
        items: T[A],
        f: A => F[B]
      )(implicit progressInfo: ProgressInfo[F, A]): F[T[B]] = {
        def progressWithTotal(total: Long): F[T[B]] =
          progressBuilder
            .build(InitParams("", total))
            .use(progress =>
              items.traverse { item =>
                for {
                  msg <- progressInfo.message(item)
                  _ <- progress.setExtraMessage(msg)
                  h <- f(item)
                  n <- progressInfo.step(item)
                  _ <- progress.stepBy(n)
                } yield h
              }
            )
        items
          .foldLeftM(0L) { case (acc, a) => progressInfo.step(a).map(_ + acc) }
          .flatMap(progressWithTotal)
      }

    }
}
