package ru.d10xa.file_adventure

import java.nio.file.Paths

import cats.effect.SyncIO
import ru.d10xa.file_adventure.core.FileAndHash

class ChecksumFileTest extends TestBase {
  test("parse sfv line") {
    val sum = "b0234dd3d7cf7d659a20645161c4245d61c791cb5d8e3f77f98ef6e009d0b244"
    val filename = "1@#  $%^& **)(*&^%.txt"
    val res = FileAndHash
      .fromLine[SyncIO](Paths.get(""), s"$sum *$filename")
      .unsafeRunSync()

    res.regularFile shouldBe Paths.get(filename)
    res.hash.value shouldBe sum
  }
}
