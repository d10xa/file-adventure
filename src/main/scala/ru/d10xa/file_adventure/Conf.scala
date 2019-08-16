package ru.d10xa.file_adventure

import org.rogach.scallop._

class SubcommandMinus extends Subcommand("minus") {
  val right: ScallopOption[String] = opt[String](short = 'r')
  val left: ScallopOption[String] = opt[String](short = 'l')
}

class SubcommandSha256 extends Subcommand("sha256") {
  val dir: ScallopOption[String] = opt[String]()
}

class SubcommandCreate extends Subcommand("create") {
  val dir: ScallopOption[String] = opt[String]()
}

class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
  val minus: SubcommandMinus = new SubcommandMinus()
  val sha256: SubcommandSha256 = new SubcommandSha256()
  val create: SubcommandCreate = new SubcommandCreate()

  addSubcommand(minus)
  addSubcommand(sha256)
  addSubcommand(create)
  verify()
}
