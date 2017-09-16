enablePlugins(ScalaJSPlugin)

name := "HexagonyIDE"
scalaVersion := "2.12.3"
scalacOptions += "-feature"

// This is an application with a main method
scalaJSUseMainModuleInitializer := true
//mainClass := Some("net.akouryy.hexd.HexagonyIDE")

libraryDependencies ++= Seq(
  "org.scala-js" %%% "scalajs-dom" % "0.9.1",
  "be.doeraene" %%% "scalajs-jquery" % "0.9.1",
  "com.chuusai" %%% "shapeless" % "2.3.2",
)

artifactPath in(Compile, fastOptJS) := baseDirectory.value / "js" / "hex-ide-fastopt.js"
artifactPath in(Compile, fullOptJS) := baseDirectory.value / "js" / "hex-ide-fullopt.js"
