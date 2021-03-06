val scala3Version = "3.1.0"

lazy val root = project
  .in(file("."))
  .settings(
    name         := "day04",
    version      := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.0",
      "org.typelevel"          %% "cats-core"                % "2.7.0"
    )
  )
