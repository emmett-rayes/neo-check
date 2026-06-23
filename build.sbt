scalaVersion := "3.8.4"

lazy val root = rootProject
  .settings(
    name := "NeoCheck",
    idePackagePrefix := Some("neocheck"),
    scalacOptions ++= Seq(
      "-feature",
      "-source:future",
      "-language:experimental.modularity",
      "-language:experimental.pureFunctions",
      "-Wsafe-init",
      "-Yexplicit-nulls",
    ),
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.20" % Test,
      "org.scalatestplus" %% "scalacheck-1-19" % "3.2.20.0" % Test,
    )
  )
