ThisBuild / scalaVersion := "2.13.1"
ThisBuild / version := "0.1.0"
ThisBuild / organization := "ru.d10xa"

lazy val root = (project in file(".")).settings(
  name := "file-adventure",
  assemblyJarName in assembly := "file-adventure.jar",
  mainClass in assembly := Some("ru.d10xa.file_adventure.Main"),
  test in assembly := {},
  scalacOptions := Seq(
    "-encoding",
    "UTF-8", // source files are in UTF-8
    "-deprecation", // warn about use of deprecated APIs
    "-unchecked", // warn about unchecked type parameters
    "-feature", // warn about misused language features
    "-language:higherKinds", // allow higher kinded types without `import scala.language.higherKinds`
    "-Xlint", // enable handy linter warnings
    "-Xfatal-warnings" // turn compiler warnings into errors
  ),
  addCompilerPlugin(
    ("org.typelevel" % "kind-projector_2.13.1" % "0.11.0")
  )
)

wartremoverErrors in (Compile, compile) ++= Seq(
  Wart.AnyVal,
  Wart.ArrayEquals,
  Wart.AsInstanceOf,
  Wart.DefaultArguments,
  Wart.EitherProjectionPartial,
  Wart.Enumeration,
  Wart.Equals,
  Wart.ExplicitImplicitTypes,
  Wart.FinalCaseClass,
  Wart.FinalVal,
  Wart.ImplicitConversion,
  Wart.ImplicitParameter,
  Wart.IsInstanceOf,
  Wart.JavaConversions,
  Wart.JavaSerializable,
  Wart.LeakingSealed,
  Wart.MutableDataStructures,
  Wart.NonUnitStatements,
  Wart.Null,
  Wart.Option2Iterable,
  Wart.OptionPartial,
  Wart.Overloading,
  Wart.Product,
  Wart.PublicInference,
  Wart.Recursion,
  Wart.Return,
  Wart.Serializable,
  Wart.StringPlusAny,
//  Wart.Throw,
  Wart.ToString,
  Wart.TraversableOps,
  Wart.TryPartial,
  Wart.Var,
  Wart.While
)

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % Test
libraryDependencies += "me.tongfei" % "progressbar" % "0.7.4"
libraryDependencies += "com.github.pathikrit" %% "better-files" % "3.8.0"
libraryDependencies += "commons-codec" % "commons-codec" % "1.13"
libraryDependencies += "org.rogach" %% "scallop" % "3.3.1"
libraryDependencies += "org.typelevel" %% "cats-core" % "2.0.0-RC1"
libraryDependencies += "org.tpolecat" %% "doobie-core" % "0.8.4"
libraryDependencies += "org.xerial" % "sqlite-jdbc" % "3.28.0"
