name := "EasyLSH"

version := "0.0"

scalaVersion := "2.11.7"

fork in run := true

javaOptions in run ++= Seq("-Xms2g", "-Xmx4g", "-Xmn1g", "-XX:+UseParallelGC")

scalacOptions += "-feature"

libraryDependencies ++= Seq(
    "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test",
    "org.scalanlp" %% "breeze" % "0.11.2",
    "org.scalanlp" %% "breeze-natives" % "0.11.2"
)