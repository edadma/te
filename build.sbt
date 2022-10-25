name := "te"

version := "0.0.1"

scalaVersion := "3.2.0"

enablePlugins(ScalaNativePlugin)

nativeLinkStubs := true

nativeMode := "debug"

nativeLinkingOptions := Seq(s"-L/${baseDirectory.value}/native-lib")

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-language:postfixOps",
  "-language:implicitConversions",
  "-language:existentials",
)

organization := "io.github.edadma"

githubOwner := "edadma"

githubRepository := name.value

Global / onChangedBuildSource := ReloadOnSourceChanges

resolvers += Resolver.githubPackages("edadma")

licenses := Seq("ISC" -> url("https://opensource.org/licenses/ISC"))

homepage := Some(url("https://github.com/edadma/" + name.value))

libraryDependencies ++= Seq(
  "io.github.edadma" %%% "ncurses" % "0.2.6",
)

libraryDependencies ++= Seq(
  "com.github.scopt" %%% "scopt" % "4.1.0",
)

publishMavenStyle := true

Test / publishArtifact := false
