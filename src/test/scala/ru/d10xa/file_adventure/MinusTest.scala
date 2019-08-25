package ru.d10xa.file_adventure

import better.files._

class MinusTest extends TestBase {
  test("sha for dir with one file equal to hash of file") {
    Minus
      .minus(File("src/test/file/minus/dir1"), File("src/test/file/minus/dir2"))
      .map(_.name)
      .shouldEqual(Set("c.txt"))
  }
}
