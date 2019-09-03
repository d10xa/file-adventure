package ru.d10xa.file_adventure

import better.files.File
import ru.d10xa.file_adventure.core.FileAndHash

class CoreTest extends TestBase {
  test("FileAndHash asFileString") {
    FileAndHash(
      File("a and b.txt"),
      "305a3caf57112616f807585c502d4442a987ff66f3416df01596fed2142c90b0").asFileString
      .shouldEqual(
        "305a3caf57112616f807585c502d4442a987ff66f3416df01596fed2142c90b0 *a and b.txt")
  }
  test("FileAndHash fromLine") {
    FileAndHash
      .fromLine(
        "305a3caf57112616f807585c502d4442a987ff66f3416df01596fed2142c90b0 *a and b.txt")
      .shouldEqual(
        FileAndHash(
          File("a and b.txt"),
          "305a3caf57112616f807585c502d4442a987ff66f3416df01596fed2142c90b0"))
  }
}
