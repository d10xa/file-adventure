package ru.d10xa.file_adventure

import better.files._
import ru.d10xa.file_adventure.Sha256.recursiveHash

class Sha256Test extends TestBase {
  test("sha for dir with one file equal to hash of file") {
    recursiveHash(File("src/test/file/one_file_dir"))
      .shouldEqual(
        "a948904f2f0f479b8f8197694b30184b0d2ed1c1cd2a1ec0fb85d299a192a447")
  }
  test("sha for two files") {
    recursiveHash(File("src/test/file/two_file_dir"))
      .shouldEqual(
        "9e7cc9e1540e8b9cc91eb6a7da9ec924144b2eaaae26097c6427dcf547711add")
  }
}
