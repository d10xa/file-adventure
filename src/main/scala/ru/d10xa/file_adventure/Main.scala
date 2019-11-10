package ru.d10xa.file_adventure

import better.files._
import cats.effect.ContextShift
import cats.effect.IO
import ru.d10xa.file_adventure.core.Sha256Hash
import cats.implicits._
import doobie.Transactor
import doobie.implicits._

import scala.concurrent.ExecutionContext

object Main {

  implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.global)
  val dir: String = s"${System
    .getProperty("user.home")}/.file-adventure"

  better.files.File(dir).createDirectoryIfNotExists()

  val xa: Transactor[IO] = Transactor.fromDriverManager[IO](
    "org.sqlite.JDBC",
    s"""jdbc:sqlite:$dir/fileadventure.db""",
    "",
    ""
  )

  val sortHashes: Vector[Sha256Hash] => Vector[Sha256Hash] =
    xs => xs.sortBy(_.asBigInteger)

  val sortedHashesToSingleHash: Vector[Sha256Hash] => Sha256Hash = {
    case Vector(x: Sha256Hash) => x
    case xs => Sha256Hash.fromString(xs.map(_.value.show).mkString(""))
  }

  def main(args: Array[String]): Unit = {

    val _ = sql"""
CREATE TABLE IF NOT EXISTS files (
path text NOT NULL,
sha256 text NOT NULL,
verified_at integer NOT NULL,
PRIMARY KEY (path, sha256)
)""".update.run.transact(xa).unsafeRunSync
    val conf = new Conf(args.toList)

    conf.subcommand match {
      case Some(conf.minus) =>
        new Minus(File(conf.minus.left()), File(conf.minus.right())).run()
      case Some(conf.sha256) =>
        new Sha256(File(conf.sha256.dir())).run()
      case Some(conf.create) =>
        new Create(
          File(conf.create.dir()),
          new DoobieFileEntryRepositoryInterpreter(xa)).run()
      case Some(conf.check) =>
        new Check(File(conf.check.dir())).run()
      case _ => ()
    }
  }

}
