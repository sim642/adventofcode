name := "adventofcode"

version := "0.1"

scalaVersion := "3.2.0"
scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-explain",
  "-explain-types",
)

// TODO: unnecessary for scalameter because it exists on maven central?
//resolvers += "Sonatype OSS Snapshots" at
//  "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.14"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.14" % "test"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.17.0" % "test"
libraryDependencies += "org.scalatestplus" %% "scalacheck-1-17" % "3.2.14.0" % "test"
libraryDependencies += ("org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1").cross(CrossVersion.for3Use2_13) // for scalameter 2.13
libraryDependencies += "com.lihaoyi" %% "ujson" % "2.0.0"
libraryDependencies += ("com.storm-enroute" %% "scalameter" % "0.19" % "test").cross(CrossVersion.for3Use2_13) // 0.21 has transitive io.github.... dependency which breaks io.Source in tests
  //.exclude("org.scala-lang.modules", "scala-parser-combinators_2.13") // scalameter 2.13 dependency workaround
  .exclude("org.scala-lang.modules", "scala-xml_2.13") // scalameter 2.13 dependency workaround
// libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "2.0.0-RC1" // scalameter 2.13 dependency workaround

// TODO: scalameter tests don't work in SBT
//testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")
//logBuffered := false
//parallelExecution in Test := false

