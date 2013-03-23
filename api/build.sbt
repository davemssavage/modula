name := "modula-api"

version := "1.0"

scalaVersion := "2.10.0"

libraryDependencies ++= Seq(
  "org.mockito" % "mockito-core" % "1.9.5" % "test",
  "org.specs2" % "specs2_2.10" % "1.14" % "test",
  "org.scalacheck" % "scalacheck_2.10" % "1.10.0" % "test",
  "junit" % "junit" % "4.11" % "test",
  "com.novocode" % "junit-interface" % "0.8" % "test"
)
