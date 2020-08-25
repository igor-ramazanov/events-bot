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

libraryDependencies += "org.typelevel" %% "simulacrum" % "1.0.0"
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"

libraryDependencies += "co.fs2" % "fs2-core_2.13" % "2.2.2"
libraryDependencies += "co.fs2" % "fs2-io_2.13" % "2.2.1"
libraryDependencies += "com.chuusai" % "shapeless_2.13" % "2.3.3"
libraryDependencies += "com.twitter" % "hpack" % "1.0.2"
libraryDependencies += "io.chrisdavenport" % "log4cats-core_2.13" % "1.0.1"
libraryDependencies += "io.chrisdavenport" % "log4cats-slf4j_2.13" % "1.0.1"
libraryDependencies += "io.chrisdavenport" % "unique_2.13" % "2.0.0"
libraryDependencies += "io.chrisdavenport" % "vault_2.13" % "2.0.0"
libraryDependencies += "io.circe" % "circe-core_2.13" % "0.13.0-RC1"
libraryDependencies += "io.circe" % "circe-generic_2.13" % "0.12.3"
libraryDependencies += "io.circe" % "circe-jawn_2.13" % "0.13.0-RC1"
libraryDependencies += "io.circe" % "circe-numbers_2.13" % "0.13.0-RC1"
libraryDependencies += "io.circe" % "circe-parser_2.13" % "0.12.3"
libraryDependencies += "org.eclipse.jetty.alpn" % "alpn-api" % "1.1.3.v20160715"
libraryDependencies += "org.http4s" % "blaze-core_2.13" % "0.14.11"
libraryDependencies += "org.http4s" % "blaze-http_2.13" % "0.14.11"
libraryDependencies += "org.http4s" % "http4s-blaze-client_2.13" % "0.21.0-RC2"
libraryDependencies += "org.http4s" % "http4s-blaze-core_2.13" % "0.21.0-RC2"
libraryDependencies += "org.http4s" % "http4s-blaze-server_2.13" % "0.21.0-RC2"
libraryDependencies += "org.http4s" % "http4s-circe_2.13" % "0.21.0-RC2"
libraryDependencies += "org.http4s" % "http4s-client_2.13" % "0.21.0-RC2"
libraryDependencies += "org.http4s" % "http4s-core_2.13" % "0.21.0-RC2"
libraryDependencies += "org.http4s" % "http4s-dsl_2.13" % "0.21.0-RC2"
libraryDependencies += "org.http4s" % "http4s-jawn_2.13" % "0.21.0-RC2"
libraryDependencies += "org.http4s" % "http4s-server_2.13" % "0.21.0-RC2"
libraryDependencies += "org.http4s" % "jawn-fs2_2.13" % "1.0.0-RC2"
libraryDependencies += "org.http4s" % "parboiled_2.13" % "2.0.1"
libraryDependencies += "org.log4s" % "log4s_2.13" % "1.8.2"
libraryDependencies += "org.scala-lang" % "scala-library" % "2.13.1"
libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.13.0"
libraryDependencies += "org.scodec" % "scodec-bits_2.13" % "1.1.12"
libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.28"
libraryDependencies += "org.typelevel" % "cats-core_2.13" % "2.1.0"
libraryDependencies += "org.typelevel" % "cats-effect_2.13" % "2.1.0"
libraryDependencies += "org.typelevel" % "cats-kernel_2.13" % "2.1.0"
libraryDependencies += "org.typelevel" % "cats-macros_2.13" % "2.1.0"
libraryDependencies += "org.typelevel" % "jawn-parser_2.13" % "1.0.0"

dockerUsername := Some("igorramazanov")
dockerUpdateLatest := true
dockerExposedPorts := Seq(8443)
dockerBaseImage := "openjdk:14-alpine"
