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

  trait TraverseProgressImplicits {
    implicit class TraverseProgressOps[T[_]: Traverse, A](ta: T[A]) {
      def traverseWithProgress[F[_]: TraverseProgress, B](
        afb: A => F[B]
      )(implicit progressInfo: ProgressInfo[F, A]): F[T[B]] =
        TraverseProgress[F].traverseWithProgress(ta, afb)
    }
  }

  def apply[F[_]](implicit instance: TraverseProgress[F]): TraverseProgress[F] =
    instance

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
