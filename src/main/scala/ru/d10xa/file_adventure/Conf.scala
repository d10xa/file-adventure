package ru.d10xa.file_adventure

final case class MinusCommand(left: String, right: String)
final case class Sha256Command(dir: String)
final case class CreateCommand(dir: String)
final case class CheckCommand(dir: String)
