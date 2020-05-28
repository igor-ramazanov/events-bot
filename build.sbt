name := "events-bot"
version := "0.1"
organization := "igorramazanov.tech"
maintainer := "Igor Ramazanov <igor.ramazanov@protonmail.com>"

scalaVersion := "2.13.2"

scalacOptions += "-Ymacro-annotations"

enablePlugins(JavaAppPackaging)
enablePlugins(AshScriptPlugin)
enablePlugins(DockerPlugin)

addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
addCompilerPlugin(
  ("org.typelevel" %% "kind-projector" % "0.11.0").cross(CrossVersion.full)
)

Global / cancelable := false

libraryDependencies += "org.augustjune" %% "canoe" % "0.4.1"
libraryDependencies += "org.typelevel" %% "simulacrum" % "1.0.0"
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"

dockerUsername := Some("igorramazanov")
dockerUpdateLatest := true
dockerBaseImage := "openjdk:14-alpine"
