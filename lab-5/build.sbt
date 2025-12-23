lazy val root = project
  .in(file("."))
  .settings(
    name := "lab5",
    scalaVersion := "3.5.2",
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.2" % Test
  )
