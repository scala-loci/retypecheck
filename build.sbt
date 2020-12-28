enablePlugins(GitVersioning)

ThisBuild / git.useGitDescribe := true

ThisBuild / name := "retypecheck"

ThisBuild / organization := "de.tuda.stg"

ThisBuild / licenses += "Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")

ThisBuild / scalacOptions ++= Seq("-feature", "-deprecation", "-unchecked", "-Xlint")

ThisBuild / scalaVersion := "2.13.4"


def checkVersion(version: String)(check: PartialFunction[(Long, Long, Long), Boolean]) = {
  val components = raw"""(\d{1,19})\.(\d{1,19})\.(\d{1,19}).*""".r
  version match {
    case components(major, minor, patch) =>
      check.lift((major.toLong, minor.toLong, patch.toLong)) getOrElse false
    case _ =>
      false
  }
}

def `is 2.12+`(scalaVersion: String): Boolean =
  checkVersion(scalaVersion) { case (2, m, _) => m >= 12 }

def `is 2.12.2+`(scalaVersion: String): Boolean =
  checkVersion(scalaVersion) { case (2, m, p) => m >= 13 || m >= 12 && p >= 2 }

def `is 2.13+`(scalaVersion: String): Boolean =
  checkVersion(scalaVersion) { case (2, m, _) => m >= 13 }

def `is 2.13.2+`(scalaVersion: String): Boolean =
  checkVersion(scalaVersion) { case (2, m, p) => m >= 13 && p >= 2 }


val dependencies = libraryDependencies ++= Seq(
  scalaOrganization.value % "scala-reflect" % scalaVersion.value % Provided,
  "org.scalamacros" %% "resetallattrs" % "1.0.0")

val dependenciesTest = libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.3" % Test,
  if (`is 2.12+`(scalaVersion.value))
    "org.parboiled" %% "parboiled" % "2.2.1" % Test
  else
    "org.parboiled" %% "parboiled" % "2.1.8" % Test,
  if (`is 2.12+`(scalaVersion.value))
    "org.scala-lang.modules" %% "scala-async" % "0.10.0" % Test
  else
    "org.scala-lang.modules" %% "scala-async" % "0.9.7" % Test,
  "org.scalatest" %% "scalatest" % "3.2.3" % Test)

val macroAnnotation = Seq(
  scalacOptions ++= {
    if (`is 2.13+`(scalaVersion.value))
      Seq("-Ymacro-annotations")
    else
      Seq.empty
  },
  libraryDependencies ++= {
    if (`is 2.13+`(scalaVersion.value))
      Seq.empty
    else if (`is 2.12.2+`(scalaVersion.value))
      Seq(compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.patch))
    else
      Seq(compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.patch))
  })

val base = ThisBuild / baseDirectory


def projectWithBaseSrc(project: Project) = project settings (
  Compile / unmanagedSourceDirectories ++= {
    if (`is 2.13.2+`(scalaVersion.value))
      Seq(base.value / "src" / "main" / "scala-2.13.2+")
    else
      Seq(base.value / "src" / "main" / "scala-2.13.1-")
  },
  Compile / unmanagedSourceDirectories += base.value / "src" / "main" / "scala",
  Compile / unmanagedResourceDirectories += base.value / "src" / "main" / "resources",
  Test / unmanagedSourceDirectories ++= {
    if (`is 2.13.2+`(scalaVersion.value))
      Seq(base.value / "src" / "test" / "scala-2.13.2+")
    else
      Seq(base.value / "src" / "test" / "scala-2.13.1-")
  },
  Test / unmanagedSourceDirectories += base.value / "src" / "test" / "scala",
  Test / unmanagedResourceDirectories += base.value / "src" / "test" / "resources")

def mainProject(project: Project) = projectWithBaseSrc(project) settings (
  scalaVersion := "2.13.1",
  crossScalaVersions := Seq("2.11.12", "2.12.12", "2.13.4"),
  dependencies)

def testProject(project: Project) = projectWithBaseSrc(project) settings (
  skip in publish := true,
  scalaVersion := "2.13.1",
  crossScalaVersions := Seq(
    "2.11.6", "2.11.7", "2.11.8", "2.11.9", "2.11.10", "2.11.11", "2.11.12",
    "2.12.0", "2.12.1", "2.12.2", "2.12.3", "2.12.4", "2.12.5", "2.12.6", "2.12.7", "2.12.8", "2.12.9", "2.12.10", "2.12.11", "2.12.12",
    "2.13.0", "2.13.1", "2.13.2", "2.13.3", "2.13.4"),
  dependencies,
  dependenciesTest)


lazy val retypecheck = (project in file(".")
  settings (
    skip in publish := true,
    Compile / unmanagedSourceDirectories := Seq.empty,
    Compile / unmanagedResourceDirectories := Seq.empty,
    Test / unmanagedSourceDirectories := Seq.empty,
    Test / unmanagedResourceDirectories := Seq.empty)
  aggregate (main, testMacro, testMacroAnnotation))

lazy val main = (mainProject(project) in file(".main")
  settings (
    normalizedName := "retypecheck",
    Compile / unmanagedSources / excludeFilter := "TyperTester.scala",
    Test / unmanagedSources / excludeFilter := "*"))

lazy val testMacro = (testProject(project) in file(".test-macro"))

lazy val testMacroAnnotation = (testProject(project) in file(".test-macro-annotation")
  settings (macroAnnotation))
