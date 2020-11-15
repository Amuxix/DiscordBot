name := "DiscordBot"

version := "0.4"

scalaVersion := "2.13.3"

// format: off
javacOptions ++= Seq("-Xlint", "-encoding", "UTF-8")
scalacOptions ++= Seq(
  "-encoding", "utf-8",                // Specify character encoding used by source files.
  "-explaintypes",                     // Explain type errors in more detail.
  "-language:higherKinds",             // Allow higher-kinded types
  "-language:postfixOps",              // Explicitly enables the postfix ops feature
  "-language:implicitConversions",     // Explicitly enables the implicit conversions feature
  "-Ybackend-parallelism", "12",       // Maximum worker threads for backend.
  "-Ybackend-worker-queue", "10",      // Backend threads worker queue size.
  "-Ymacro-annotations",               // Enable support for macro annotations, formerly in macro paradise.
  "-Xcheckinit",                       // Wrap field accessors to throw an exception on uninitialized access.
  "-Xmigration:2.14.0",                // Warn about constructs whose behavior may have changed since version.
  "-Werror",                           // Fail the compilation if there are any warnings.
  "-Xlint:adapted-args",               // Warn if an argument list is modified to match the receiver.
  "-Xlint:inaccessible",               // Warn when nullary methods return Unit.
  "-Xlint:infer-any",                  // Warn when a type argument is inferred to be Any.
  "-Xlint:doc-detached",               // Warn when nullary methods return Unit.
  "-Xlint:implicit-not-found",         // Check @implicitNotFound and @implicitAmbiguous messages.
  "-Xlint:deprecation",                // Enable linted deprecations.
  "-deprecation",                      // Emit warning and location for usages of deprecated APIs.
  "-unchecked",                        // Enable additional warnings where generated code depends on assumptions.
  "-feature",                          // Emit warning and location for usages of features that should be imported explicitly.
  "-Wdead-code",                       // Warn when dead code is identified.
  "-Wextra-implicit",                  // Warn when more than one implicit parameter section is defined.
  "-Woctal-literal",                   // Warn on obsolete octal syntax.
  "-Wunused:explicits",                // Warn if an explicit parameter is unused.
  "-Wunused:implicits",                // Warn if an implicit parameter is unused.
  "-Wunused:privates",                 // Warn if a private member is unused.
  "-Wunused:locals",                   // Warn if a local definition is unused.
  "-Wunused:patvars",                  // Warn if a variable bound in a pattern is unused.
  "-Wunused:params",                   // Enable -Wunused:explicits,implicits. Warn if an explicit/implicit parameter is unused.
  "-Wunused:linted",                   // -Xlint:unused <=> Enable -Wunused:imports,privates,locals,implicits.
  "-Wvalue-discard",                   // Warn when non-Unit expression results are unused.
)
// format: on

resolvers += "jcenter-bintray" at "https://jcenter.bintray.com"

enablePlugins(JavaAppPackaging)
enablePlugins(DockerPlugin)

val http4sVersion = "0.21.8"
libraryDependencies ++= Seq(
  "net.dv8tion"            % "JDA"                 % "4.2.0_214",
  "com.github.pureconfig" %% "pureconfig"          % "0.14.0",
  "org.typelevel"         %% "cats-effect"         % "2.2.0",
  "co.fs2"                %% "fs2-core"            % "2.4.4",
  "org.http4s"            %% "http4s-dsl"          % http4sVersion,
  "org.http4s"            %% "http4s-blaze-client" % http4sVersion,
)

javaOptions in Universal ++= Seq(
  "-Dconfig.file=/data/conf/application.conf",
)

dockerRepository in Docker := Some("amuxix")

dockerUpdateLatest in Docker := true
