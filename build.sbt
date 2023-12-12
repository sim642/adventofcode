name := "adventofcode"

version := "0.1"

scalaVersion := "3.3.1"
scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-explain",
  "-explain-types",
)

// TODO: unnecessary for scalameter because it exists on maven central?
//resolvers += "Sonatype OSS Snapshots" at
//  "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.17"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.17" % "test"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.17.0" % "test"
libraryDependencies += "org.scalatestplus" %% "scalacheck-1-17" % "3.2.17.0" % "test"
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.3.0"
libraryDependencies += "com.lihaoyi" %% "ujson" % "3.1.3"
libraryDependencies += "dk.brics" % "automaton" % "1.12-4"
libraryDependencies += "io.github.hughsimpson" %% "scalameter" % "0.22.1" % "test" // Scala 3 compatible scalameter fork

// TODO: scalameter tests don't work in SBT
//testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")
//logBuffered := false
//parallelExecution in Test := false

