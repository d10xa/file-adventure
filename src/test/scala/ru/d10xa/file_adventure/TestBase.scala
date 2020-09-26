package ru.d10xa.file_adventure

import cats.effect.IO
import cats.effect.Resource
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import ru.d10xa.file_adventure.Progress.ProgressBuilder
import ru.d10xa.file_adventure.fs.Checksum
import ru.d10xa.file_adventure.fs.Fs

abstract class TestBase extends AnyFunSuite with Matchers {
  implicit val checksum: Checksum[IO] =
    Checksum.make[IO].unsafeRunSync()
  implicit val console: Console[IO] =
    Console.make[IO].unsafeRunSync()
  val progressBuilder: ProgressBuilder[IO] = new ProgressBuilder[IO] {
    override def build(
      params: Progress.InitParams
    ): Resource[IO, Progress[IO]] =
      Resource.pure(new Progress[IO] {
        override def setExtraMessage(msg: String): IO[Unit] = IO.unit

        override def stepBy(n: Long): IO[Unit] = IO.unit
      })
  }
  val fs: Fs[IO] = Fs.make[IO].unsafeRunSync()
}
