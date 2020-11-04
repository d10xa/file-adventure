package ru.d10xa.file_adventure

import java.nio.file.Path
import java.nio.file.Paths

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
      name = "fsumo",
      header = "hash sum command line utility"
    ) {

  val sortHashes: Vector[Sha256Hash] => Vector[Sha256Hash] =
    xs => xs.sortBy(_.asBigInteger)

  val sortedHashesToSingleHash: Vector[Sha256Hash] => Sha256Hash = {
    case Vector(x: Sha256Hash) => x
    case xs => Sha256Hash.fromString(xs.map(_.value.show).mkString(""))
  }

  val debugOpt: Opts[Boolean] = Opts.flag("debug", "log in debug mode").orFalse

  val minusOpts: Opts[MinusCommand] =
    Opts.subcommand("minus", help = "l - r")(
      (
        Opts.option[Path]("left", short = "l", help = "left folder"),
        Opts.option[Path]("right", short = "r", help = "right folder")
      ).mapN(MinusCommand.apply)
    )

  val sha256Opts: Opts[Sha256Command] =
    Opts.subcommand(
      "sha256",
      help =
        "calculate sha256 of folder (concatenate hashes of files and calculate hash of hashes)"
    )(
      Opts
        .argument[Path]("path")
        .withDefault(Paths.get(""))
        .map(Sha256Command.apply)
    )

  val createOpts: Opts[CreateCommand] =
    Opts.subcommand(
      "create",
      help = s"create ${core.FILESUM_CONSTANT_NAME} files"
    )(
      (
        Opts
          .arguments[Path]("path")
          .withDefault(NonEmptyList.one(Paths.get("."))),
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
      (
        Opts
          .argument[Path]("path"),
        debugOpt
      )
        .mapN(CheckCommand.apply)
    )

  val compareOpts: Opts[CompareCommand] =
    Opts.subcommand(
      "compare",
      help = "compare two directories by checksum files"
    )(
      (
        Opts.option[Path]("left", help = "left path"),
        Opts.option[Path]("right", "right path")
      )
        .mapN(CompareCommand.apply)
    )

  override def main: Opts[IO[ExitCode]] = program[IO]

  def program[F[_]: Sync]: Opts[F[ExitCode]] = {
    val minus = minusOpts.map(runCommand[F])
    val sha256 = sha256Opts.map(runCommand[F])
    val create = createOpts.map(runCommand[F])
    val check = checkOpts.map(runCommand[F])
    val compare = compareOpts.map(runCommand[F])
    minus.orElse(sha256).orElse(create).orElse(check).orElse(compare)
  }

  def runCommand[F[_]: Sync](c: CommandBase): F[ExitCode] =
    Context
      .make[F](c match {
        case c: CheckCommand => c.debug
        case _ => false
      })
      .flatMap(_.run(c))
      .as(ExitCode.Success)

}
