package ru.d10xa.file_adventure

import java.nio.file.Path
import java.nio.file.Paths

import cats.effect.IO

class CheckTest extends TestBase {

  test("check ok") {
    val checkedFiles = check("ok")
    checkedFiles.size shouldBe 1
    val Vector(ok) = checkedFiles
    ok.valid shouldBe true
  }

  test("check fail") {
    val checkedFiles = check("fail")
    checkedFiles.size shouldBe 1
    val Vector(failed) = checkedFiles
    failed.valid shouldBe false
  }

  test("check fail recursive") {
    val checkedFiles = check("fail_recursive")
    checkedFiles.size shouldBe 2
    val Vector(ok, failed) = checkedFiles
    ok.valid shouldBe true
    failed.valid shouldBe false
  }

  test("check ok recursive") {
    val checkedFiles = check("ok_recursive")
    checkedFiles.size shouldBe 2
    val Vector(ok, failed) = checkedFiles
    ok.valid shouldBe true
    failed.valid shouldBe true
  }

  def check(path: String): Vector[CheckedFile] =
    checkFullPath(Paths.get(s"src/test/file/check/$path"))

  def checkFullPath(d: Path): Vector[CheckedFile] =
    new Check[IO]().checkDir(d).unsafeRunSync()

}
