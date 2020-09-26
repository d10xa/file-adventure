package ru.d10xa.file_adventure

import better.files.File
import cats.effect.IO

class CheckTest extends TestBase {

  test("check ok") {
    val checkedFiles = checkDir(File("src/test/file/check/ok"))
    checkedFiles.size shouldBe 1
    val file = checkedFiles.head
    file.valid shouldBe true
  }

  test("check fail") {
    val checkedFiles = checkDir(File("src/test/file/check/fail"))
    checkedFiles.size shouldBe 1
    val file = checkedFiles.head
    file.valid shouldBe false
  }

  def checkDir(d: File): Vector[CheckedFile] =
    new Check[IO](fs).checkDir(d).unsafeRunSync()

}
