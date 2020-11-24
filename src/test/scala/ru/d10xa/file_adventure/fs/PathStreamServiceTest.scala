package ru.d10xa.file_adventure.fs

import java.nio.file.Files
import java.nio.file.Paths

import ru.d10xa.file_adventure.TestBase
import ru.d10xa.file_adventure.implicits._
import cats.syntax.all._

class PathStreamServiceTest extends TestBase {
  val root = "src/test/file/path_stream_service"

  test("stream only existent files") {
    isFile(s"$root/with_sfv/.sha256.sfv").shouldBe(true)
    isFile(s"$root/with_sfv/without_sfv/.sha256.sfv")
      .shouldBe(false)
    isFile(
      s"$root/with_sfv/without_sfv/with_sfv/.sha256.sfv"
    ).shouldBe(true)

    val set = pathStreamService
      .outerWalk(
        Paths.get(s"$root/with_sfv/without_sfv/with_sfv"),
        ".sha256.sfv"
      )
      .map(_.path.show)
      .compile
      .to(Set)
      .unsafeRunSync()
    set should contain("../../.sha256.sfv")
    set should not contain ("../.sha256.sfv")
    set should not contain ("../../../.sha256.sfv")
  }

  def isFile(path: String): Boolean =
    Files.isRegularFile(Paths.get(path))

}
