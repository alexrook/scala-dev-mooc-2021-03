import sbt.Keys.libraryDependencies

scalacOptions += "-Ymacro-annotations"

lazy val root = (project in file("."))
  .settings(
    name := "scala-dev-mooc-2021-03",
    version := "0.1",
    scalaVersion := "2.13.3",
    libraryDependencies ++= Dependencies.zio,
    libraryDependencies ++= Dependencies.zioConfig,
    libraryDependencies ++= Dependencies.doobie,
    libraryDependencies ++= Dependencies.http4sServer,
    libraryDependencies ++= Dependencies.circe,
    libraryDependencies ++= Dependencies.pureconfig,
    libraryDependencies ++= Seq(
      Dependencies.kindProjector,
      Dependencies.logback,
      Dependencies.liquibase,
      Dependencies.postgres
    ),
    addCompilerPlugin(Dependencies.kindProjector))

testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework"))