package ru.d10xa.file_adventure

import java.nio.file.Path

import cats.Show

trait CatsInstances {
  implicit val catsShowForPath: Show[Path] = Show.fromToString
}

object implicits extends CatsInstances
