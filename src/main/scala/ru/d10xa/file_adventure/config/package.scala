package ru.d10xa.file_adventure

import io.circe._

package object config {
  implicit val circeDecoderDatabaseConfig: Decoder[DatabaseConfig] =
    Decoder.forProduct1("url")(url => DatabaseConfig(url))
  implicit val circeDecoderFileAdventureConfig: Decoder[FileAdventureConfig] =
    Decoder.forProduct1("db")(db => FileAdventureConfig(db))
}
