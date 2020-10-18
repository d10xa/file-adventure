ThisBuild / scalaVersion := "2.13.3"
ThisBuild / organization := "ru.d10xa"

lazy val root = (project in file("."))
  .settings(commonSettings: _*)
  .settings(
    name := "file-adventure",
    assemblyJarName in assembly := "file-adventure.jar",
    mainClass in assembly := Some("ru.d10xa.file_adventure.Main"),
    test in assembly := {},
    scalacOptions ++= Seq(
      "-encoding",
      "UTF-8", // source files are in UTF-8
      "-deprecation", // warn about use of deprecated APIs
      "-unchecked", // warn about unchecked type parameters
      "-feature", // warn about misused language features
      "-language:higherKinds", // allow higher kinded types without `import scala.language.higherKinds`
      "-Xlint", // enable handy linter warnings
      "-Wvalue-discard",
      "-Xfatal-warnings" // turn compiler warnings into errors
//    "-Xlint:byname-implicit" // https://github.com/scala/bug/issues/12072
    ),
    scalacOptions in (Compile, console) ~= {
      _.filterNot(Set("-Xfatal-warnings"))
    },
    scalacOptions in (Test, console) := (scalacOptions in (Compile, console)).value,
    addCompilerPlugin(
      ("org.typelevel" %% "kind-projector" % "0.11.0").cross(CrossVersion.full)
    ),
    addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
  )
  .enablePlugins(GitVersioning)

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
//  Wart.ImplicitParameter,
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
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.0" % Test
libraryDependencies += "me.tongfei" % "progressbar" % "0.8.1"
libraryDependencies += "com.github.pathikrit" %% "better-files" % "3.9.1"
libraryDependencies += "commons-codec" % "commons-codec" % "1.14"
libraryDependencies += "org.typelevel" %% "cats-core" % "2.1.1"
libraryDependencies += "io.circe" %% "circe-config" % "0.8.0"
libraryDependencies += "io.circe" %% "circe-generic" % "0.13.0"
libraryDependencies += "com.monovore" %% "decline" % "1.2.0"
libraryDependencies += "com.monovore" %% "decline-effect" % "1.2.0"

lazy val commonSettings = Seq(
  bintrayVcsUrl := Some("https://github.com/d10xa/file-adventure.git"),
  licenses += ("MIT", url("http://opensource.org/licenses/MIT"))
)
