import AssemblyKeys._ // put this at the top of the file

assemblySettings

name := "modula-main"

version := "1.0"

scalaVersion := "2.10.0"

libraryDependencies ++= Seq(
  "org.osgi" % "org.osgi.core" % "5.0.0",
  "junit" % "junit" % "4.11" % "test",
  "org.mockito" % "mockito-core" % "1.9.5" % "test",
  "com.novocode" % "junit-interface" % "0.8" % "test"
)
