ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.1"

val fs2Version = "3.9.2"
val catsVersion = "3.5.2"
val http4sVersion = "0.23.30"
val circeVersion = "0.14.6"
val fs2DataVersion = "1.11.2"

lazy val root = (project in file("."))
  .settings(
    name := "pokerToolsV3",
    libraryDependencies ++= Seq(
      "org.http4s" %% "http4s-ember-client" % http4sVersion,
      "org.http4s" %% "http4s-ember-server" % http4sVersion,
      "org.http4s" %% "http4s-dsl" % http4sVersion,
      "org.http4s" %% "http4s-circe" % http4sVersion,
      "io.circe" %% "circe-generic" % circeVersion,
      "io.circe" %% "circe-core" % circeVersion,
      "io.circe" %% "circe-parser" % circeVersion,
      // "io.circe" %% "circe-fs2" % "0.14.1",
      "co.fs2" %% "fs2-core" % fs2Version,
      "co.fs2" %% "fs2-io" % fs2Version,
      "org.typelevel" %% "cats-effect" % catsVersion,
      "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4",
      // "org.typelevel" %% "log4cats-core"    % "2.6.0",  // Only if you want to Support Any Backend,
      "org.typelevel" %% "log4cats-slf4j" % "2.6.0", // Direct Slf4j Support - Recommended
      // "ch.qos.logback" % "logback-classic" % "1.4.7",
      "org.tpolecat" %% "skunk-core" % "0.6.2",
      "org.tpolecat" %% "skunk-circe" % "0.6.2",
      "com.github.nscala-time" %% "nscala-time" % "2.32.0",
      "org.scalatest" %% "scalatest" % "3.2.19" % "test",
      "org.tpolecat" %% "skunk-core" % "0.6.4",
      "org.gnieh" %% "fs2-data-json" % fs2DataVersion,
      "org.gnieh" %% "fs2-data-json-circe" % fs2DataVersion
    )
  )

scalacOptions += "-Xmax-inlines:64"
