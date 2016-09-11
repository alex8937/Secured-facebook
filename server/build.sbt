name := "FacebookApp"

version := "1.0"

scalaVersion := "2.11.7"

resolvers += "spray repo" at "http://repo.spray.io"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += "RoundEights" at "http://maven.spikemark.net/roundeights"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

libraryDependencies ++= {
  val akkaV = "2.4.1"
  val sprayV = "1.3.2"
  Seq(
    "io.spray"            %%  "spray-can"     % sprayV,
    "io.spray"            %%  "spray-routing" % sprayV,
    "io.spray"		  %%  "spray-client"  % sprayV,
    "io.spray"		  %%  "spray-json"  % "1.3.1",
    "com.typesafe.akka"   %%  "akka-actor"    % akkaV,
    "com.typesafe.akka"   %%  "akka-remote"    % akkaV,
    "org.specs2"          %%  "specs2-core"   % "2.3.11" % "test"
  )
}
libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.2"
libraryDependencies += "commons-lang" % "commons-lang" % "2.6"
libraryDependencies += "commons-codec" % "commons-codec" % "1.9"
libraryDependencies += "com.roundeights" %% "hasher" % "1.2.0"
