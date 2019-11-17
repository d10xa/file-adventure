package ru.d10xa.file_adventure.config

final case class FileAdventureConfig(db: DatabaseConfig)

final case class DatabaseConfig(url: String)
