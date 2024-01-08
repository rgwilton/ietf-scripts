val scalaVer = "3.3.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "ietf-scripts",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scalaVer,
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0-M6" % Test
  )