enablePlugins(ScalaJSPlugin)

name := "HexagonyIDE"
scalaVersion := "2.12.3" // or any other Scala version >= 2.10.2
scalacOptions += "-feature"

// This is an application with a main method
scalaJSUseMainModuleInitializer := true
//mainClass := Some("net.akouryy.hexd.HexagonyIDE")

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.1"
libraryDependencies += "be.doeraene" %%% "scalajs-jquery" % "0.9.1"

artifactPath in(Compile, fastOptJS) := baseDirectory.value / "js" / "hex-ide-fastopt.js"
artifactPath in(Compile, fullOptJS) := baseDirectory.value / "js" / "hex-ide-fullopt.js"
