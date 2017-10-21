name := "scala-playground"

version := "0.1"

scalaVersion := "2.12.1"

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.12" % "3.0.4" % "test",
  "org.scala-lang" % "scala-actors" % "2.11.11",
  "org.scalaz" %% "scalaz-core" % "7.2.16"
)
        