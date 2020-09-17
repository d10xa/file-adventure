package ru.d10xa.file_adventure

import better.files.File
import cats.effect.IO

class MinusTest extends TestBase {
  test("sha for dir with one file equal to hash of file") {
    new Minus[IO](progressBuilder)
      .minus(File("src/test/file/minus/dir1"), File("src/test/file/minus/dir2"))
      .unsafeRunSync()
      .map(_.name)
      .shouldEqual(Set("c.txt"))
  }
}
