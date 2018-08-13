ThisBuild / scalaVersion := "2.12.6"
ThisBuild / organization := "com.example"
lazy val demo = (project in file(".")).settings(
  name := "Demo",
  resolvers += Resolver.bintrayRepo("cibotech", "public"),
  libraryDependencies += "com.cibo" %% "evilplot" % "0.4.1", // Use %%% instead of %% if you're using ScalaJS
  resolvers += Resolver.bintrayRepo("cibotech", "public"),
  libraryDependencies += "com.cibo" %% "scalastan" % "0.5.8"
)