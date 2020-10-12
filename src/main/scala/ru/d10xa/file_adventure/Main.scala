package ru.d10xa.file_adventure

import cats.data.NonEmptyList
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.Sync
import ru.d10xa.file_adventure.core.Sha256Hash
import cats.implicits._
import com.monovore.decline._
import com.monovore.decline.effect._

object Main
    extends CommandIOApp(
      name = "file-adventure",
      header = "hash sum command line utility"
    ) {

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
      ).mapN(MinusCommand.apply)
    )

  val sha256Opts: Opts[Sha256Command] =
    Opts.subcommand(
      "sha256",
      help =
        "calculate sha256 of folder (concatenate hashes of files and calculate hash of hashes)"
    )(
      Opts
        .argument[String]("path")
        .withDefault("")
        .map(Sha256Command.apply)
    )

  val createOpts: Opts[CreateCommand] =
    Opts.subcommand(
      "create",
      help = s"create ${core.FILESUM_CONSTANT_NAME} files"
    )(
      (
        Opts
          .arguments[String]("path")
          .withDefault(NonEmptyList.one(".")),
        Opts
          .flag(
            "one-file",
            help = "make a single file with deep listing in it"
          )
          .orFalse
      )
        .mapN(CreateCommand.apply)
    )

  val checkOpts: Opts[CheckCommand] =
    Opts.subcommand(
      "check",
      help = s"check directory corresponds ${core.FILESUM_CONSTANT_NAME} file"
    )(
      Opts
        .argument[String]("path")
        .map(CheckCommand.apply)
    )

  override def main: Opts[IO[ExitCode]] = program[IO]

  def program[F[_]: Sync]: Opts[F[ExitCode]] = {
    val minus = minusOpts.map(runCommand[F])
    val sha256 = sha256Opts.map(runCommand[F])
    val create = createOpts.map(runCommand[F])
    val check = checkOpts.map(runCommand[F])
    minus.orElse(sha256).orElse(create).orElse(check)
  }

  def runCommand[F[_]: Sync](c: CommandBase): F[ExitCode] =
    Context
      .make[F]
      .flatMap(_.run(c))
      .as(ExitCode.Success)

}
