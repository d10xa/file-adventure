package ru.d10xa.file_adventure

import java.nio.file.Path

import cats.data.NonEmptyList

sealed trait CommandBase
final case class MinusCommand(left: Path, right: Path) extends CommandBase
final case class Sha256Command(dir: Path) extends CommandBase
final case class CreateCommand(dirs: NonEmptyList[Path], oneFile: Boolean)
    extends CommandBase
final case class CheckCommand(dir: Path, debug: Boolean) extends CommandBase
final case class CompareCommand(left: Path, right: Path) extends CommandBase
