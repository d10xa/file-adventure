package ru.d10xa.file_adventure.progress

import cats.effect.Resource
import cats.effect.Sync
import cats.implicits._
import me.tongfei.progressbar.ProgressBar

trait Progress[F[_]] {
  def setExtraMessage(msg: String): F[Unit]
  def stepBy(n: Long): F[Unit]
}

object Progress {

  trait ProgressBuilder[F[_]] {
    def build(params: InitParams): Resource[F, Progress[F]]
  }

  object ProgressBuilder {
    def apply[F[_]](implicit instance: ProgressBuilder[F]): ProgressBuilder[F] =
      instance
  }

  final case class InitParams(task: String, initialMax: Long)

  private def toProgress[F[_]: Sync](pb: ProgressBar): F[Progress[F]] =
    Sync[F].delay(new Progress[F] {
      override def setExtraMessage(msg: String): F[Unit] =
        Sync[F].delay(pb.setExtraMessage(msg)).void
      override def stepBy(n: Long): F[Unit] = Sync[F].delay(pb.stepBy(n)).void
    })

  def builder[F[_]: Sync]: F[ProgressBuilder[F]] =
    Sync[F].delay((params: InitParams) =>
      Resource
        .fromAutoCloseable(
          Sync[F].delay(
            new ProgressBar(params.task, params.initialMax)
          )
        )
        .flatMap(pb => Resource.liftF(toProgress(pb)))
    )

}
