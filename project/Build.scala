import sbt._
import Keys._

object ModulaBuild extends Build {
    lazy val root = Project(id = "modula",
                            base = file(".")) settings (ScctPlugin.mergeReportSettings: _*) aggregate(api, impl, factory, osgi, main)

    lazy val api = Project(id = "modula-api",
                           base = file("api")) settings (ScctPlugin.instrumentSettings: _*)

    lazy val impl = Project(id = "modula-impl",
                           base = file("impl")) settings (ScctPlugin.instrumentSettings: _*) dependsOn(api)

    lazy val tools = Project(id = "modula-tools",
                           base = file("tools")) settings (ScctPlugin.instrumentSettings: _*)

    lazy val factory = Project(id = "modula-factory",
                           base = file("factory")) settings (ScctPlugin.instrumentSettings: _*) dependsOn(api, impl)

    lazy val osgi = Project(id = "modula-osgi",
                           base = file("osgi")) settings (ScctPlugin.instrumentSettings: _*) dependsOn(api, factory, impl, tools)

    lazy val main = Project(id = "modula-main",
                           base = file("main")) settings (ScctPlugin.instrumentSettings: _*) dependsOn(api, factory, osgi)
}
