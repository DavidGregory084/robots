import scala.xml.{ Elem, Node, NodeSeq }
import scala.xml.transform.{ RewriteRule, RuleTransformer }

lazy val robots = project.in(file("."))
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
    scalacOptions.in(Tut) ~= { _.filterNot(Set("-Ywarn-unused:imports", "-Ywarn-unused-import", "-Ywarn-dead-code")) },
    tutTargetDirectory := file(".")
  )

lazy val docs = project.in(file("docs"))
  .enablePlugins(MicrositesPlugin)
  .enablePlugins(ScalaUnidocPlugin)
  .settings(commonSettings)
  .settings(publishSettings)
  .settings(noPublishSettings)
  .settings(
    moduleName := "robots-docs",

    ghpagesNoJekyll := false,
    git.remoteRepo := "git@github.com:DavidGregory084/robots.git",
    addMappingsToSiteDir(mappings.in(ScalaUnidoc, packageDoc), micrositeDocumentationUrl),

    scalacOptions.in(Tut) ~= { _.filterNot(Set("-Ywarn-unused:imports", "-Ywarn-unused-import", "-Ywarn-dead-code")) },

    scalacOptions in (ScalaUnidoc, unidoc) ++= Seq(
      "-groups",
      "-implicits",
      "-sourcepath", baseDirectory.in(LocalRootProject).value.getAbsolutePath,
      "-doc-source-url", scmInfo.value.get.browseUrl + "/tree/master€{FILE_PATH}.scala",
      "-doc-root-content", (resourceDirectory.in(Compile).value / "rootdoc.txt").getAbsolutePath
    ),

    micrositeName := "Robots",
    micrositeDescription := "A helper library for validating data with Cats",
    micrositeAuthor := "David Gregory",
    micrositeHomepage := "https://DavidGregory084.github.io/robots",
    micrositeBaseUrl := "/robots",
    micrositeDocumentationUrl := "api",
    micrositeGithubOwner := "DavidGregory084",
    micrositeGithubRepo := "robots",
    micrositeExtraMdFiles := Map(file("README.md") -> microsites.ExtraMdFileConfig("index.md", "home")),
    micrositePalette := Map(
      "brand-primary"     -> "#424242",
      "brand-secondary"   -> "#7A7A7A",
      "brand-tertiary"    -> "#3F3F3F",
      "gray-dark"         -> "#453E46",
      "gray"              -> "#837F84",
      "gray-light"        -> "#E3E2E3",
      "gray-lighter"      -> "#F4F3F4",
      "white-color"       -> "#FFFFFF"),
    includeFilter in makeSite := "*.html" | "*.css" | "*.png" | "*.jpg" | "*.gif" | "*.js" | "*.swf" | "*.yml" | "*.md"
  )

lazy val commonSettings = Seq(
  organization := "io.github.davidgregory084",

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
  pomIncludeRepository := Function.const(false),
  autoAPIMappings := true,
  apiURL := Some(url("https://DavidGregory084.github.io/robots/api/")),

  homepage := Some(url("https://github.com/DavidGregory084/robots")),

  licenses := Seq("Apache 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),

  scmInfo := Some(ScmInfo(
    url("https://github.com/DavidGregory084/robots"),
    "scm:git:git@github.com:DavidGregory084/robots.git"
  )),

  developers := List(Developer(
    "DavidGregory084", "David Gregory",
    "davidgregory084@gmail.com",
    url("https://twitter.com/DavidGregory084")
  )),

  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases" at nexus + "service/local/staging/deploy/maven2")
  },

  credentials ++= (for {
    username <- Option(System.getenv().get("SONATYPE_USERNAME"))
    password <- Option(System.getenv().get("SONATYPE_PASSWORD"))
  } yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)).toSeq,

  pomPostProcess := { (node: Node) =>
    new RuleTransformer(new RewriteRule {
      override def transform(node: Node): NodeSeq = node match {
        case elem: Elem =>
          val isDependency = elem.label == "dependency"
          val isInTestScope = elem.child.exists(c => c.label == "scope" && c.text == "test")

          if (isDependency && isInTestScope)
            Nil
          else
            elem

        case _ =>
          node
      }
    }).transform(node).head
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
      publishArtifacts,
      setNextVersion,
      commitNextVersion,
      ReleaseStep(action = Command.process("sonatypeReleaseAll", _), enableCrossBuild = true),
      pushChanges
    )
  }
)

lazy val noPublishSettings = Seq(
  publish := (),
  publishLocal := (),
  publishArtifact := false
)
