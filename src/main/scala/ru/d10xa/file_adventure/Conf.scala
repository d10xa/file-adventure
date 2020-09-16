package ru.d10xa.file_adventure

sealed trait CommandBase
final case class MinusCommand(left: String, right: String) extends CommandBase
final case class Sha256Command(dir: String) extends CommandBase
final case class CreateCommand(dir: String, oneFile: Boolean)
    extends CommandBase
final case class CheckCommand(dir: String) extends CommandBase
