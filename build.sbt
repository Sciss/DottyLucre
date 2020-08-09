lazy val root = project.in(file("."))
  .settings(
    name         := "DottyLucre",
    version      := "0.1.0-SNAPSHOT",
    scalaVersion := "0.26.0-RC1",
    libraryDependencies ++= Seq(
      "de.sciss" %% "serial" % "1.1.3-SNAPSHOT"
    )
  )
