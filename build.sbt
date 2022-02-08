lazy val root = (project in file("."))
  .settings(
    scalaVersion := "3.1.1",
    name := "hkdms",
    version := "0.1",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.7.0",
      "org.scalatest" %% "scalatest" % "3.2.10" % "test"
    )
  )
