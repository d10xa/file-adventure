package ru.d10xa.file_adventure

import better.files._
import ru.d10xa.file_adventure.core.Sha256Hash
import cats.implicits._

object Main {

  val sortHashes: Vector[Sha256Hash] => Vector[Sha256Hash] =
    xs => xs.sortBy(_.asBigInteger)

  val sortedHashesToSingleHash: Vector[Sha256Hash] => Sha256Hash = {
    case Vector(x: Sha256Hash) => x
    case xs => Sha256Hash.fromString(xs.map(_.value.show).mkString(""))
  }

  def main(args: Array[String]): Unit = {
    val conf = new Conf(args.toList)

    conf.subcommand match {
      case Some(conf.minus) =>
        new Minus(File(conf.minus.left()), File(conf.minus.right())).run()
      case Some(conf.sha256) =>
        new Sha256(File(conf.sha256.dir())).run()
      case Some(conf.create) =>
        new Create(File(conf.create.dir())).run()
      case Some(conf.check) =>
        new Check(File(conf.check.dir())).run()
      case _ => ()
    }
  }

}
