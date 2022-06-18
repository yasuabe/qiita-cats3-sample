val scala3Version = "3.1.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "qiita-cats3-sample",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,
    scalacOptions ++= List("-Ykind-projector:underscores"),

    libraryDependencies ++= List(
      "org.typelevel"     %% "cats-core"        % "2.6.1",
      "org.typelevel"     %% "cats-free"        % "2.7.0",
      "org.typelevel"     %% "cats-effect"      % "3.3.11",
      "eu.timepit"        %% "refined"          % "0.9.29",
      "org.typelevel"     %% "algebra"          % "2.7.0",
      "org.typelevel"     %% "squants"          % "1.8.3",
      "io.circe"          %% "circe-core"       % "0.14.2",
      "io.circe"          %% "circe-parser"     % "0.14.2",
      "io.circe"          %% "circe-refined"    % "0.14.2",
      "io.circe"          %% "circe-generic"    % "0.14.2",
      "org.typelevel"     %% "algebra-laws"     % "2.7.0"  % Test,
      "org.scalameta"     %% "munit"            % "0.7.29" % Test,
      "org.scalacheck"    %% "scalacheck"       % "1.16.0" % Test,
      "org.scalameta"     %% "munit-scalacheck" % "0.7.29" % Test,
      "io.chrisdavenport" %% "cats-scalacheck"  % "0.3.1"  % Test
    )
  )