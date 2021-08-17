name := "te"

version := "0.1.0"

scalaVersion := "2.13.6"

enablePlugins(ScalaNativePlugin)

nativeLinkStubs := true

nativeMode := "debug"

nativeLinkingOptions := Seq(s"-L/${baseDirectory.value}/native-lib")

scalacOptions ++= Seq("-deprecation",
                      "-feature",
                      "-unchecked",
                      "-language:postfixOps",
                      "-language:implicitConversions",
                      "-language:existentials")

organization := "xyz.hyperreal"

githubOwner := "edadma"

githubRepository := name.value

Global / onChangedBuildSource := ReloadOnSourceChanges

resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"

resolvers += Resolver.githubPackages("edadma")

Compile / mainClass := Some("xyz.hyperreal." + name.value.replace('-', '_') + ".Main")

licenses := Seq("ISC" -> url("https://opensource.org/licenses/ISC"))

homepage := Some(url("https://github.com/edadma/" + name.value))

libraryDependencies ++= Seq(
  "xyz.hyperreal" %%% "ncurses" % "0.1.0-snapshot.23"
)

libraryDependencies ++= Seq(
  "com.github.scopt" %%% "scopt" % "4.0.1"
)

publishMavenStyle := true

Test / publishArtifact := false

pomIncludeRepository := { _ =>
  false
}

pomExtra :=
  <scm>
    <url>git@github.com:edadma/{name.value}.git</url>
    <connection>scm:git:git@github.com:edadma/{name.value}.git</connection>
  </scm>
    <developers>
      <developer>
        <id>edadma</id>
        <name>Edward A. Maxedon, Sr.</name>
        <url>https://github.com/edadma</url>
      </developer>
    </developers>
