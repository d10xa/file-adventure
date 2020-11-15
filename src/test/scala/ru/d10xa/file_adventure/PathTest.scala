package ru.d10xa.file_adventure

import java.nio.file.Paths

class PathTest extends TestBase {
  import ru.d10xa.file_adventure.implicits._

  test("isParentFor") {
    val abc = Paths.get("a/b/c")
    val abcd = Paths.get("a/b/c/d")
    abc.isParentFor(abcd) shouldBe true
    abcd.isParentFor(abc) shouldBe false
  }

}
