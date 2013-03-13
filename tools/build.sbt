name := "modula-tools"

version := "1.0"

scalaVersion := "2.10.0"

libraryDependencies ++= Seq(
  "org.osgi" % "org.osgi.core" % "5.0.0",
  "com.google.guava" % "guava" % "13.0.1",
  "junit" % "junit" % "4.11" % "test",
  "org.mockito" % "mockito-core" % "1.9.5" % "test",
  "com.novocode" % "junit-interface" % "0.8" % "test"
)
