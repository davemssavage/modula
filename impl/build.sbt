name := "modula-impl"

version := "1.0"

scalaVersion := "2.10.0"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-actors" % "2.10.0",
  "cglib" % "cglib" % "2.2.2",
  "org.objenesis" % "objenesis" % "1.2",
  "junit" % "junit" % "4.11" % "test",
  "org.mockito" % "mockito-core" % "1.9.5" % "test",
  "com.novocode" % "junit-interface" % "0.8" % "test"
)
