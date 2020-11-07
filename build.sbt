name := "DiscordBot"

version := "0.1"

scalaVersion := "2.13.3"

resolvers += "jcenter-bintray" at "https://jcenter.bintray.com"

libraryDependencies ++= Seq(
  "net.dv8tion" % "JDA" % "4.2.0_214",
  "com.github.pureconfig" %% "pureconfig" % "0.14.0",
  "org.typelevel" %% "cats-effect" % "2.2.0",
  "co.fs2" %% "fs2-core" % "2.4.4",
)