ThisBuild / scalaVersion := "2.12.7"
javacOptions ++= Seq("-source", "1.8")

lazy val fpinscala = (project in file("."))
  .settings(
    name := "fpinscala",
    libraryDependencies += "com.typesafe.play" %% "play-json" % "2.6.9",
    libraryDependencies += "com.eed3si9n" %% "gigahorse-okhttp" % "0.3.1",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test,
  )