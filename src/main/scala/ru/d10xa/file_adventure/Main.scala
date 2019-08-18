package ru.d10xa.file_adventure

import java.math.BigInteger

import better.files._
import org.apache.commons.codec.digest.DigestUtils.sha256Hex

object Main {

  val sortHashes: Vector[String] => Vector[String] =
    _.sortBy(new BigInteger(_, 16))

  val sortedHashesToSingleHash: Vector[String] => String = {
    case Vector(x: String) => x
    case xs => sha256Hex(xs.mkString(""))
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
      case _ => ()
    }
  }

}
