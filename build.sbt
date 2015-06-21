version := "1.0-SNAPSHOT"

name := "yas-validator"

scalaVersion := "2.11.6"

resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"

libraryDependencies ++= Seq("org.specs2" %% "specs2-core" % "3.6.1" % "test")

scalacOptions in Test ++= Seq("-Yrangepos")