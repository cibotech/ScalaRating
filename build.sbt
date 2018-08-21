ThisBuild / scalaVersion := "2.12.6"
ThisBuild / organization := "com.example"

lazy val licenseSettings = Seq(
  homepage := Some(url("https://www.github.com/cibotech/ScalaRating")),
  startYear := Some(2018),
  description := "NBA team rating as calculated in ScalaStan.",
  headerLicense := Some(HeaderLicense.BSD3Clause("2018", "CiBO Technologies, Inc."))
)

lazy val demo = (project in file(".")).settings(
  name := "Demo",
  resolvers += Resolver.bintrayRepo("cibotech", "public"),
  libraryDependencies += "com.cibo" %% "evilplot" % "0.4.1", // Use %%% instead of %% if you're using ScalaJS
  resolvers += Resolver.bintrayRepo("cibotech", "public"),
  libraryDependencies += "com.cibo" %% "scalastan" % "0.5.8",
  resolvers += Resolver.bintrayRepo("cibotech", "public"),
  libraryDependencies += "com.cibo" %% "evilplot-repl" % "0.4.1"
).settings(licenseSettings)