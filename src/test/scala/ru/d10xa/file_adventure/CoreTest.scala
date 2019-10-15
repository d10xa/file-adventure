package ru.d10xa.file_adventure

import better.files.File
import ru.d10xa.file_adventure.core.FileAndHash
import ru.d10xa.file_adventure.core.Sha256Hash

class CoreTest extends TestBase {
  test("FileAndHash asFileString") {
    val line =
      "305a3caf57112616f807585c502d4442a987ff66f3416df01596fed2142c90b0 *a and b.txt"
    val fileName = "a and b.txt"
    val hash =
      "305a3caf57112616f807585c502d4442a987ff66f3416df01596fed2142c90b0"

    FileAndHash(File(fileName), Sha256Hash(hash)).asFileString
      .shouldEqual(line)
  }

  test("FileAndHash fromLine") {
    val line =
      "305a3caf57112616f807585c502d4442a987ff66f3416df01596fed2142c90b0 *a and b.txt"
    val hash =
      "305a3caf57112616f807585c502d4442a987ff66f3416df01596fed2142c90b0"
    val fileName = "a and b.txt"

    FileAndHash
      .fromLine(File("."))(line)
      .shouldEqual(FileAndHash(File(fileName), Sha256Hash(hash)))
  }
}
