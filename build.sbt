name := "shapeless-applied"
organization := "io.github.jeremyrsmith"
version := "0.1.0-SNAPSHOT"

lazy val scala213 = "2.13.3"
lazy val scala212 = "2.11.12"
lazy val scala211 = "2.12.11"

scalaVersion := scala213

crossScalaVersions := Seq(
  scala213, scala212, scala211
)

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.4.0-M1",
  scalaOrganization.value % "scala-reflect" % scalaVersion.value,
  "org.scalatest" %% "scalatest" % "3.2.0" % Test
)

scalacOptions ++= Seq(
  "-deprecation"
)