package ru.d10xa.file_adventure

import cats.effect.IO
import cats.effect.Resource
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import ru.d10xa.file_adventure.progress.Progress.ProgressBuilder
import ru.d10xa.file_adventure.fs.Checksum
import ru.d10xa.file_adventure.fs.Fs
import ru.d10xa.file_adventure.progress.Progress
import ru.d10xa.file_adventure.progress.TraverseProgress

abstract class TestBase extends AnyFunSuite with Matchers {
  implicit val checksum: Checksum[IO] =
    Checksum.make[IO].unsafeRunSync()
  implicit val console: Console[IO] =
    Console.make[IO].unsafeRunSync()
  implicit val log: Log[IO] =
    Log.make[IO](debug = true)
  implicit val progressBuilder: ProgressBuilder[IO] = new ProgressBuilder[IO] {
    override def build(
      params: Progress.InitParams
    ): Resource[IO, Progress[IO]] =
      Resource.pure(new Progress[IO] {
        override def setExtraMessage(msg: String): IO[Unit] = IO.unit

        override def stepBy(n: Long): IO[Unit] = IO.unit
      })
  }
  implicit val traverseProgress: TraverseProgress[IO] =
    TraverseProgress.make[IO](progressBuilder)
  implicit val fs: Fs[IO] = Fs.make[IO].unsafeRunSync()
  val sfvReader: SfvReader[IO] = SfvReader.make[IO]
}
