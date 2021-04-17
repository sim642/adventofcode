name := "adventofcode"

version := "0.1"

scalaVersion := "2.13.5"
scalacOptions ++= Seq("-deprecation")

// TODO: unnecessary for scalameter because it exists on maven central?
//resolvers += "Sonatype OSS Snapshots" at
//  "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.0" % "test"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"
libraryDependencies += "org.scalatestplus" %% "scalacheck-1-14" % "3.2.2.0" % "test"
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
libraryDependencies += "com.lihaoyi" %% "ujson" % "0.7.5"
libraryDependencies += "com.storm-enroute" %% "scalameter" % "0.19" % "test"

// TODO: scalameter tests don't work in SBT
//testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")
//logBuffered := false
//parallelExecution in Test := false

