import sbt.Keys.libraryDependencies

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.4.0"


lazy val root = (project in file("."))
  .settings(
    name := "GrokkingFp-Scala",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % "3.5.3",
      "org.apache.jena"    % "apache-jena-libs" % "4.10.0",
      "org.apache.jena"    % "jena-fuseki-main" % "4.10.0",
      "org.slf4j"          % "slf4j-nop"        % "2.0.12",
      "org.scalatest"     %% "scalatest"        % "3.2.18"   % Test,
      "org.scalatestplus" %% "scalacheck-1-16"  % "3.2.14.0" % Test,
    )
  )
