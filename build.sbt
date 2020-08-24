lazy val baseName = "DottyLucre"

lazy val commonSettings = Seq(
  version      := "0.1.0-SNAPSHOT",
  scalaVersion := "0.26.0-RC1",
  crossScalaVersions := Seq("0.26.0-RC1"), // forget it :-(   : "2.13.3"
  scalacOptions += "-Yerased-terms",
)

lazy val root = project.in(file("."))
  .aggregate(base, data)
  .settings(
    name := baseName,
  )

lazy val base = project.in(file("base"))
  .settings(commonSettings)
  .settings(
    name := s"$baseName-base",
    libraryDependencies ++= Seq(
//      "de.sciss" %% "serial" % "1.1.3-SNAPSHOT"
    )
  )

lazy val data = project.in(file("data"))
  .dependsOn(base)
  .settings(commonSettings)
  .settings(
    name := s"$baseName-data",
    libraryDependencies ++= Seq(
    )
  )
