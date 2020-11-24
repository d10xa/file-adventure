package ru.d10xa.file_adventure

import java.util.concurrent.Executors

import cats.effect.ContextShift
import cats.effect.IO
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import ru.d10xa.file_adventure.progress.Progress.ProgressBuilder
import ru.d10xa.file_adventure.fs.Checksum
import ru.d10xa.file_adventure.fs.Fs
import ru.d10xa.file_adventure.fs.PathStreamService
import ru.d10xa.file_adventure.progress.Progress
import ru.d10xa.file_adventure.progress.TraverseProgress

import scala.concurrent.ExecutionContext

abstract class TestBase extends AnyFunSuite with Matchers {
  implicit val ec: ContextShift[IO] = IO.contextShift(
    ExecutionContext.fromExecutorService(
      Executors
        .newFixedThreadPool(5)
    )
  )
  implicit val checksum: Checksum[IO] =
    Checksum.make[IO].unsafeRunSync()
  implicit val console: Console[IO] =
    Console.make[IO].unsafeRunSync()
  implicit val log: Log[IO] =
    Log.make[IO](debug = true)
  implicit val progressBuilder: ProgressBuilder[IO] =
    Progress.dummyBuilder[IO].unsafeRunSync()
  implicit val traverseProgress: TraverseProgress[IO] =
    TraverseProgress.make[IO](progressBuilder)
  implicit val fs: Fs[IO] = Fs.make[IO].unsafeRunSync()
  val sfvReader: SfvReader[IO] = SfvReader.make[IO]
  val pathStreamService: PathStreamService[IO] = new PathStreamService[IO]
}
