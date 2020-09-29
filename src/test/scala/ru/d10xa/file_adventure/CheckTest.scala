package ru.d10xa.file_adventure

import java.nio.file.Path
import java.nio.file.Paths

import cats.effect.IO

class CheckTest extends TestBase {

  test("check ok") {
    val checkedFiles = checkDir(Paths.get("src/test/file/check/ok"))
    checkedFiles.size shouldBe 1
    val file = checkedFiles.head
    file.valid shouldBe true
  }

  test("check fail") {
    val checkedFiles = checkDir(Paths.get("src/test/file/check/fail"))
    checkedFiles.size shouldBe 1
    val file = checkedFiles.head
    file.valid shouldBe false
  }

  def checkDir(d: Path): Vector[CheckedFile] =
    new Check[IO](fs).checkDir(d).unsafeRunSync()

}
