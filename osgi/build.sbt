name := "modula-osgi"

version := "1.0"

scalaVersion := "2.10.0"

libraryDependencies ++= Seq(
  "org.osgi" % "org.osgi.core" % "5.0.0",
  "junit" % "junit" % "4.11" % "test",
  "org.mockito" % "mockito-core" % "1.9.5" % "test",
  "com.novocode" % "junit-interface" % "0.8" % "test",
  "org.junit" % "com.springsource.org.junit" % "4.11.0" % "test"
)

//resolvers += Resolver.url("spring-bundle-repo", url("http://repository.springsource.com/ivy/bundles/external/"))( Patterns("[organisation]/[module]/[revision]/[artifact]-[revision].[ext]") )

resolvers += Resolver.url("my-test-repo").artifacts("http://repository.springsource.com/ivy/bundles/external/[organisation]/[module]/[revision]/[artifact]-[revision].[ext]").ivys("http://repository.springsource.com/ivy/bundles/external/[organisation]/[module]/[revision]/[artifact]-[revision].[ext]")
