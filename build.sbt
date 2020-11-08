lazy val root = (project in file("."))
  .settings(commonSettings: _*)
  .settings(
    name := "file-adventure",
    scalacOptions ++= Seq(
      "-encoding",
      "UTF-8", // source files are in UTF-8
      "-deprecation", // warn about use of deprecated APIs
      "-unchecked", // warn about unchecked type parameters
      "-feature", // warn about misused language features
      "-language:higherKinds", // allow higher kinded types without `import scala.language.higherKinds`
      "-Xlint", // enable handy linter warnings
      "-Wvalue-discard"
    ),
    addCompilerPlugin(
      ("org.typelevel" %% "kind-projector" % "0.11.0").cross(CrossVersion.full)
    ),
    addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
    libraryDependencies ++= Seq(
      "ch.qos.logback" % "logback-classic" % "1.2.3",
      "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2",
      "org.scalatest" %% "scalatest" % "3.2.0" % Test,
      "me.tongfei" % "progressbar" % "0.8.1",
      "com.github.pathikrit" %% "better-files" % "3.9.1",
      "commons-codec" % "commons-codec" % "1.14",
      "org.typelevel" %% "cats-core" % "2.1.1",
      "io.circe" %% "circe-config" % "0.8.0",
      "io.circe" %% "circe-generic" % "0.13.0",
      "com.monovore" %% "decline" % "1.2.0",
      "com.monovore" %% "decline-effect" % "1.2.0",
      "co.fs2" %% "fs2-core" % "2.4.4",
      "co.fs2" %% "fs2-io" % "2.4.4"
    )
  )
  .enablePlugins(GitVersioning)

lazy val commonSettings = Seq(
  scalaVersion := "2.13.3",
  organization := "ru.d10xa",
  bintrayVcsUrl := Some("https://github.com/d10xa/file-adventure.git"),
  licenses += ("MIT", url("http://opensource.org/licenses/MIT"))
)
