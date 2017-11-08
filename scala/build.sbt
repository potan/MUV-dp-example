name := "json template"

version := "0.1"

scalaVersion := "2.12.3" //"2.11.8"

resolvers += Resolver.sonatypeRepo("snapshots")

//addCompilerPlugin("org.psywerx.hairyfotr" %% "linter" % "0.1-SNAPSHOT")

mainClass in Compile := Some("muv.Main")
//mainClass in Compile := Some("muv_cppst.Main")

//libraryDependencies += "org.scala-lang" % "scala-parser-combinators" % "latest.integration" //"2.11.0-M4"
//libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "latest.integration" //"1.0.5"
//libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.4"
