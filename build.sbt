name := """Character Sheets New Website"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.1"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache,
  ws,
  "joda-time" % "joda-time" % "2.3",
  "com.itextpdf" % "itextpdf" % "5.4.3",
  "org.apache.commons" % "commons-email" % "1.2"
)