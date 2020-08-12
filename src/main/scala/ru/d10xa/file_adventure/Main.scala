package ru.d10xa.file_adventure

import better.files._
import cats.effect.ContextShift
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.Sync
import ru.d10xa.file_adventure.core.Sha256Hash
import cats.implicits._
import com.monovore.decline._
import com.monovore.decline.effect._
import doobie.util.transactor.Transactor
import ru.d10xa.file_adventure.config.FileAdventureConfig

import scala.concurrent.ExecutionContext

object Main
    extends CommandIOApp(
      name = "file-adventure",
      header = "hash sum command line utitily") {

  implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

  val dir: String = s"${System
    .getProperty("user.home")}/.file-adventure"

  better.files.File(dir).createDirectoryIfNotExists()

  val sortHashes: Vector[Sha256Hash] => Vector[Sha256Hash] =
    xs => xs.sortBy(_.asBigInteger)

  val sortedHashesToSingleHash: Vector[Sha256Hash] => Sha256Hash = {
    case Vector(x: Sha256Hash) => x
    case xs => Sha256Hash.fromString(xs.map(_.value.show).mkString(""))
  }

  val minusOpts: Opts[MinusCommand] =
    Opts.subcommand("minus", help = "l - r")(
      (
        Opts.option[String]("left", short = "l", help = "left folder"),
        Opts.option[String]("right", short = "r", help = "right folder")
      ).mapN(MinusCommand.apply))

  val sha256Opts: Opts[Sha256Command] =
    Opts.subcommand(
      "sha256",
      help =
        "calculate sha256 of folder (concatenate hashes of files and calculate hash of hashes)")(
      Opts
        .option[String]("dir", help = "directory for hash")
        .map(Sha256Command.apply)
    )

  val createOpts: Opts[CreateCommand] =
    Opts.subcommand(
      "create",
      help = s"create ${core.FILESUM_CONSTANT_NAME} files"
    )(
      Opts
        .option[String]("dir", help = "directory for hash calculations")
        .map(CreateCommand.apply)
    )

  val checkOpts: Opts[CheckCommand] =
    Opts.subcommand(
      "check",
      help = s"check directory corresponds ${core.FILESUM_CONSTANT_NAME} file"
    )(
      Opts
        .option[String]("dir", help = "directory to check")
        .map(CheckCommand.apply)
    )

  override def main: Opts[IO[ExitCode]] = program[IO]

  def program[F[_]: Sync]: Opts[F[ExitCode]] = {
    val minus = minusOpts
      .map(c => {
        Sync[F]
          .delay(new Minus(File(c.left), File(c.right)).run())
          .as(ExitCode.Success)
      })
    val sha256 = sha256Opts.map(c =>
      Sync[F].delay(new Sha256(File(c.dir)).run()).as(ExitCode.Success))
    val create = createOpts.map { c =>
      val config: FileAdventureConfig =
        io.circe.config.parser
          .decodeF[IO, FileAdventureConfig]
          .unsafeRunSync // TODO
      val xa: Transactor[IO] = Transactor.fromDriverManager[IO](
        "org.sqlite.JDBC",
        config.db.url,
        "",
        ""
      )
      import doobie.implicits._
      val _ = sql"""
//CREATE TABLE IF NOT EXISTS files (
//path text NOT NULL,
//sha256 text NOT NULL,
//verified_at integer NOT NULL,
//PRIMARY KEY (path, sha256)
//)""".update.run.transact(xa).unsafeRunSync()
      Sync[F]
        .delay(
          new Create(File(c.dir), new DoobieFileEntryRepositoryInterpreter(xa))
            .run()
        )
        .as(ExitCode.Success)
    }
    val check = checkOpts.map(c =>
      Sync[F].delay(new Check(File(c.dir)).run()).as(ExitCode.Success))
    minus.orElse(sha256).orElse(create).orElse(check)
  }

}
