package ru.d10xa.file_adventure

import java.nio.file.Paths

import cats.effect.IO
import ru.d10xa.file_adventure.core.FileAndHash
import ru.d10xa.file_adventure.core.Sha256Hash

class CoreTest extends TestBase {
  test("FileAndHash asFileString") {
    val line =
      "305a3caf57112616f807585c502d4442a987ff66f3416df01596fed2142c90b0 *a and b.txt"
    val fileName = "a and b.txt"
    val hash =
      "305a3caf57112616f807585c502d4442a987ff66f3416df01596fed2142c90b0"

    FileAndHash(Paths.get(fileName), Sha256Hash(hash)).asFileString
      .shouldEqual(line)
  }

  test("FileAndHash fromLine") {
    val line =
      "305a3caf57112616f807585c502d4442a987ff66f3416df01596fed2142c90b0 *a and b.txt"
    val hash =
      "305a3caf57112616f807585c502d4442a987ff66f3416df01596fed2142c90b0"
    val fileName = "a and b.txt"

    FileAndHash
      .fromLine[IO](Paths.get("."), line)
      .unsafeRunSync()
      .shouldEqual(FileAndHash(Paths.get(fileName), Sha256Hash(hash)))
  }
}
