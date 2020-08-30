name := "events-bot"
version := "0.1"
organization := "igorramazanov.tech"
maintainer := "Igor Ramazanov <igor.ramazanov@protonmail.com>"

scalaVersion := "2.13.3"

scalacOptions += "-Ymacro-annotations"

enablePlugins(JavaAppPackaging)
enablePlugins(AshScriptPlugin)
enablePlugins(DockerPlugin)

addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
addCompilerPlugin(
  ("org.typelevel" %% "kind-projector" % "0.11.0").cross(CrossVersion.full)
)

Global / cancelable := false

libraryDependencies ++= List(
  // Published locally, hasn't been released yet
  "org.augustjune" %% "canoe" % "0.5.0-RC1+2-203c5423-SNAPSHOT",
  "org.typelevel" %% "simulacrum" % "1.0.0",
  "org.tpolecat" %% "doobie-core" % "0.9.0",
  "org.xerial" % "sqlite-jdbc" % "3.32.3.2",
  "org.flywaydb" % "flyway-core" % "6.5.5"
)

dockerUsername := Some("igorramazanov")
dockerUpdateLatest := true
dockerExposedPorts := Seq(8443)
dockerBaseImage := "openjdk:14-alpine"
