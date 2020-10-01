package ru.d10xa.file_adventure

import java.nio.file.Paths

import cats.effect.IO
import ru.d10xa.file_adventure.implicits._

class MinusTest extends TestBase {
  test("sha for dir with one file equal to hash of file") {
    new Minus[IO](fs)
      .minus(
        Paths.get("src/test/file/minus/dir1"),
        Paths.get("src/test/file/minus/dir2")
      )
      .unsafeRunSync()
      .map(_.nameOrEmpty)
      .shouldEqual(Set("c.txt"))
  }
}
