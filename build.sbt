
name := "ShapelessConfig"

version := "1.0"

scalaVersion := "2.12.1"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats" % "0.9.0",
  "com.chuusai" %% "shapeless" % "2.3.2",
  "com.typesafe" % "config" % "1.3.1",
  "org.specs2" %% "specs2-core" % "3.8.9" % "test"
)
