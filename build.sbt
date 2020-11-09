name := "DiscordBot"

version := "0.1"

scalaVersion := "2.13.3"

resolvers += "jcenter-bintray" at "https://jcenter.bintray.com"

val http4sVersion = "0.21.8"
libraryDependencies ++= Seq(
  "net.dv8tion"           % "JDA"                  % "4.2.0_214",
  "com.github.pureconfig" %% "pureconfig"          % "0.14.0",
  "org.typelevel"         %% "cats-effect"         % "2.2.0",
  "co.fs2"                %% "fs2-core"            % "2.4.4",
  "org.http4s"            %% "http4s-dsl"          % http4sVersion,
  "org.http4s"            %% "http4s-blaze-client" % http4sVersion,
)
