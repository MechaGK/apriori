lazy val root = (project in file("."))
  .settings(
    name := "apriori",
    version := "0.1",
    scalaVersion := "2.12.6",

    libraryDependencies += "nz.ac.waikato.cms.weka" % "weka-stable" % "3.8.0",
  ).enablePlugins(SbtTwirl)
