import sbt._
import Keys._

object ModulaBuild extends Build {
    lazy val root = Project(id = "modula",
                            base = file(".")) aggregate(api, impl, factory, osgi, main)

    lazy val api = Project(id = "modula-api",
                           base = file("api"))

    lazy val impl = Project(id = "modula-impl",
                           base = file("impl")) dependsOn(api)

    lazy val tools = Project(id = "modula-tools",
                           base = file("tools"))

    lazy val factory = Project(id = "modula-factory",
                           base = file("factory")) dependsOn(api, impl)

    lazy val osgi = Project(id = "modula-osgi",
                           base = file("osgi")) dependsOn(api, factory, impl, tools)

    lazy val main = Project(id = "modula-main",
                           base = file("main")) dependsOn(api, factory, osgi)
}
