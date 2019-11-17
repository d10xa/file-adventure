package ru.d10xa.file_adventure

import cats.effect.Bracket
import doobie.Transactor
import doobie._
import doobie.implicits._

private object FileEntrySql {
  def insertOrReplace(
    fh: core.FileAndHash,
    currentTimeMillis: Long): Update0 = {
    val path = fh.regularFile.toJava.getAbsolutePath
    val hash = fh.hash
    sql"insert into files (path, sha256, verified_at) values ($path, $hash, $currentTimeMillis)".update
  }
}

class DoobieFileEntryRepositoryInterpreter[F[_]: Bracket[*[_], Throwable]](
  val xa: Transactor[F])
    extends FileEntryRepositoryAlgebra[F] {

  override def saveWithHash(fh: core.FileAndHash): F[Int] =
    FileEntrySql
      .insertOrReplace(fh, System.currentTimeMillis)
      .run
      .transact(xa)
}
