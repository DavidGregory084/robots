lazy val root = project.in(file("."))
  .settings(commonSettings)
  .settings(publishSettings)
  .settings(noPublishSettings)
  .aggregate(core, docs)

lazy val core = project.in(file("core"))
  .enablePlugins(TutPlugin)
  .settings(commonSettings)
  .settings(publishSettings)
  .settings(
    moduleName := "robots-core",
    scalacOptions.in(Tut) ~= { _.filterNot(Set("-Ywarn-unused-import", "-Ywarn-dead-code")) },
    tutTargetDirectory := file(".")
  )

lazy val docs = project.in(file("docs"))
  .enablePlugins(MicrositesPlugin)
  .settings(commonSettings)
  .settings(noPublishSettings)
  .settings(
    moduleName := "robots-docs",
    scalacOptions.in(Tut) ~= { _.filterNot(Set("-Ywarn-unused-import", "-Ywarn-dead-code")) },
    micrositeName := "Robots",
    micrositeDescription := "A helper library for validating data with Cats",
    micrositeAuthor := "David Gregory",
    micrositeDocumentationUrl := "/docs",
    micrositeHomepage := "http://DavidGregory084.github.io/robots",
    micrositePushSiteWith := GitHub4s,
    micrositeGithubOwner := "DavidGregory084",
    micrositeGithubRepo := "robots",
    micrositeGithubToken := Option(System.getenv().get("GITHUB_TOKEN")),
    micrositeExtraMdFiles := Map(file("README.md") -> microsites.ExtraMdFileConfig("index.md", "home")),
    micrositePalette := Map(
      "brand-primary"     -> "#424242",
      "brand-secondary"   -> "#7A7A7A",
      "brand-tertiary"    -> "#3F3F3F",
      "gray-dark"         -> "#453E46",
      "gray"              -> "#837F84",
      "gray-light"        -> "#E3E2E3",
      "gray-lighter"      -> "#F4F3F4",
      "white-color"       -> "#FFFFFF")
  )

lazy val commonSettings = Seq(
  organization := "io.github.robots",

  scalaVersion := "2.12.2",

  releaseCrossBuild := true,

  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core" % "0.9.0",
    "org.typelevel" %% "cats-laws" % "0.9.0" % Test,
    "org.scalatest" %% "scalatest" % "3.0.1" % Test,
    "org.scalacheck" %% "scalacheck" % "1.13.5" % Test,
    "org.typelevel" %% "discipline" % "0.7.3" % Test
  ),

  createHeaders.in(Compile) := {
    createHeaders.in(Compile).triggeredBy(compile.in(Compile)).value
  },

  addCompilerPlugin("org.spire-math" % "kind-projector" % "0.9.3" cross CrossVersion.binary),
  addCompilerPlugin("io.tryp" %% "splain" % "0.2.4"),

  // Recommendations from: http://tpolecat.github.io/2017/04/25/scalac-flags.html
  scalacOptions ++= Seq(
    "-deprecation",                      // Emit warning and location for usages of deprecated APIs.
    "-encoding", "utf-8",                // Specify character encoding used by source files.
    "-explaintypes",                     // Explain type errors in more detail.
    "-feature",                          // Emit warning and location for usages of features that should be imported explicitly.
    "-language:existentials",            // Existential types (besides wildcard types) can be written and inferred
    "-language:experimental.macros",     // Allow macro definition (besides implementation and application)
    "-language:higherKinds",             // Allow higher-kinded types
    "-language:implicitConversions",     // Allow definition of implicit functions called views
    "-unchecked",                        // Enable additional warnings where generated code depends on assumptions.
    "-Xcheckinit",                       // Wrap field accessors to throw an exception on uninitialized access.
    "-Xfatal-warnings",                  // Fail the compilation if there are any warnings.
    "-Xfuture",                          // Turn on future language features.
    "-Xlint:adapted-args",               // Warn if an argument list is modified to match the receiver.
    "-Xlint:by-name-right-associative",  // By-name parameter of right associative operator.
    "-Xlint:delayedinit-select",         // Selecting member of DelayedInit.
    "-Xlint:doc-detached",               // A Scaladoc comment appears to be detached from its element.
    "-Xlint:inaccessible",               // Warn about inaccessible types in method signatures.
    "-Xlint:infer-any",                  // Warn when a type argument is inferred to be `Any`.
    "-Xlint:missing-interpolator",       // A string literal appears to be missing an interpolator id.
    "-Xlint:nullary-override",           // Warn when non-nullary `def f()' overrides nullary `def f'.
    "-Xlint:nullary-unit",               // Warn when nullary methods return Unit.
    "-Xlint:option-implicit",            // Option.apply used implicit view.
    "-Xlint:package-object-classes",     // Class or object defined in package object.
    "-Xlint:poly-implicit-overload",     // Parameterized overloaded implicit methods are not visible as view bounds.
    "-Xlint:private-shadow",             // A private field (or class parameter) shadows a superclass field.
    "-Xlint:stars-align",                // Pattern sequence wildcard must align with sequence component.
    "-Xlint:type-parameter-shadow",      // A local type parameter shadows a type already in scope.
    "-Xlint:unsound-match",              // Pattern match may not be typesafe.
    "-Xmax-classfile-name", "240",       // Limit class file name length to ensure that classes don't break the limit in Docker's filesystem
    "-Yno-adapted-args",                 // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
    "-Ypartial-unification",             // Enable partial unification in type constructor inference
    "-Ywarn-dead-code",                  // Warn when dead code is identified.
    "-Ywarn-inaccessible",               // Warn about inaccessible types in method signatures.
    "-Ywarn-infer-any",                  // Warn when a type argument is inferred to be `Any`.
    "-Ywarn-nullary-override",           // Warn when non-nullary `def f()' overrides nullary `def f'.
    "-Ywarn-nullary-unit",               // Warn when nullary methods return Unit.
    "-Ywarn-numeric-widen",              // Warn when numerics are widened.
    "-Ywarn-value-discard"               // Warn when non-Unit expression results are unused.
  ) ++ {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, min)) if min >= 12 => List(
        "-Xlint:constant",               // Evaluation of a constant arithmetic expression results in an error.
        "-Ywarn-unused:implicits",       // Warn if an implicit parameter is unused.
        "-Ywarn-unused:imports",         // Warn if an import selector is not referenced.
        "-Ywarn-unused:locals",          // Warn if a local definition is unused.
        "-Ywarn-unused:params",          // Warn if a value parameter is unused.
        "-Ywarn-unused:patvars",         // Warn if a variable bound in a pattern is unused.
        "-Ywarn-unused:privates",        // Warn if a private member is unused.
        "-Ywarn-extra-implicit"          // Warn when more than one implicit parameter section is defined.
      )
      case Some((2, min)) if min == 11 => List(
        "-Ywarn-unused-import"          // Warn if an import selector is not referenced.
      )
      case _ => Nil
    }
  },

  scalacOptions.in(Compile, console) ~= { _.filterNot(Set("-Ywarn-unused:imports", "-Ywarn-dead-code", "-Xfatal-warnings")) },
  scalacOptions.in(Test, console) ~= { _.filterNot(Set("-Ywarn-unused:imports", "-Ywarn-dead-code", "-Xfatal-warnings")) },

  headers := {
    import de.heikoseeberger.sbtheader.license._
    Map("scala" -> Apache2_0("2017", "David Gregory and the Robots project contributors"))
  },

  unmanagedSources.in(Compile, createHeaders) ++= (sourceDirectory.in(Compile).value / "boilerplate" ** "*.template").get,

  coursierVerbosity := {
    val travisBuild = isTravisBuild.in(Global).value

    if (travisBuild)
      0
    else
      coursierVerbosity.value
  }
)

lazy val publishSettings = Seq(
  releaseCrossBuild := true,
  releasePublishArtifactsAction := PgpKeys.publishSigned.value,
  publishMavenStyle := true,
  publishArtifact.in(Test) := false,
  pomIncludeRepository := { _ => false },

  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases" at nexus + "service/local/staging/deploy/maven2")
  },

  releaseProcess := {
    import ReleaseTransformations._

    Seq[ReleaseStep](
      checkSnapshotDependencies,
      inquireVersions,
      runClean,
      runTest,
      setReleaseVersion,
      commitReleaseVersion,
      tagRelease,
      ReleaseStep(action = Command.process("publishSigned", _), enableCrossBuild = true),
      setNextVersion,
      commitNextVersion,
      ReleaseStep(action = Command.process("sonatypeReleaseAll", _), enableCrossBuild = true),
      pushChanges
    )
  },

  pomExtra := {
    <url>https://github.com/DavidGregory084/robots</url>
    <licenses>
      <license>
        <name>Apache License, Version 2.0</name>
          <url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
          <distribution>repo</distribution>
      </license>
    </licenses>
    <scm>
      <connection>scm:git:git@github.com/DavidGregory084/robots.git</connection>
      <developerConnection>scm:git:git@github.com/DavidGregory084/robots.git</developerConnection>
      <url>github.com/DavidGregory084/robots.git</url>
    </scm>
    <developers>
      <developer>
        <id>DavidGregory084</id>
        <name>David Gregory</name>
      </developer>
    </developers>
  }
)

lazy val noPublishSettings = Seq(
  publish := (),
  publishLocal := (),
  publishArtifact := false
)
