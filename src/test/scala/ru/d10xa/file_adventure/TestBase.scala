package ru.d10xa.file_adventure

import cats.effect.IO
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import ru.d10xa.file_adventure.fs.Checksum

abstract class TestBase extends AnyFunSuite with Matchers {
  implicit val checksum: Checksum[IO] =
    Checksum.make[IO].unsafeRunSync()
  implicit val console: Console[IO] =
    Console.make[IO].unsafeRunSync()
}
