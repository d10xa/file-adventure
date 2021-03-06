package ru.d10xa.file_adventure

import cats.effect.Concurrent
import cats.effect.ContextShift
import ru.d10xa.file_adventure.fs.Checksum
import ru.d10xa.file_adventure.fs.FileWrite
import cats.implicits._
import ru.d10xa.file_adventure.progress.Progress
import ru.d10xa.file_adventure.progress.Progress.ProgressBuilder
import ru.d10xa.file_adventure.fs.Fs
import ru.d10xa.file_adventure.fs.PathStreamService
import ru.d10xa.file_adventure.progress.TraverseProgress

class Context[F[_]](
  create: Create[F],
  check: Check[F],
  sha256: Sha256[F],
  minus: Minus[F],
  compare: Compare[F]
) {
  def run(cb: CommandBase): F[Unit] =
    cb match {
      case c: MinusCommand => minus.run(c)
      case c: Sha256Command => sha256.run(c)
      case c: CreateCommand => create.run(c)
      case c: CheckCommand => check.run(c)
      case c: CompareCommand => compare.run(c)
    }
}

object Context {
  def make[F[_]: ContextShift: Concurrent](
    debug: Boolean,
    progress: Boolean
  ): F[Context[F]] =
    for {
      implicit0(checksum: Checksum[F]) <- Checksum.make[F]
      implicit0(fileWrite: FileWrite[F]) <- FileWrite.make[F]
      implicit0(console: Console[F]) <- Console.make[F]
      implicit0(progressBuilder: ProgressBuilder[F]) <-
        if (progress) Progress.builder[F] else Progress.dummyBuilder[F]
      implicit0(traverseProgress: TraverseProgress[F]) =
        TraverseProgress.make[F](progressBuilder)
      implicit0(fs: Fs[F]) <- Fs.make[F]
      implicit0(log: Log[F]) = Log.make[F](debug)
      sfvReader = SfvReader.make[F]
      sfvService = SfvService.make[F]
      pathStreamService = new PathStreamService[F]
      create = new Create[F]()
      check = new Check[F](pathStreamService)
      sha256 = new Sha256[F]()
      minus = new Minus[F]()
      compare = new Compare[F](sfvReader, sfvService)
    } yield new Context[F](create, check, sha256, minus, compare)
}
