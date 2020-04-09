enablePlugins(GitVersioning)

git.useGitDescribe in ThisBuild := true

name in ThisBuild := "retypecheck"

scalaVersion in ThisBuild := "2.13.1"

organization in ThisBuild := "de.tuda.stg"

licenses in ThisBuild += "Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")

scalacOptions in ThisBuild ++= Seq("-feature", "-deprecation", "-unchecked", "-Xlint")


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


val dependencies = libraryDependencies ++= Seq(
  scalaOrganization.value % "scala-reflect" % scalaVersion.value % Provided,
  "org.scalamacros" %% "resetallattrs" % "1.0.0")

val dependenciesTest = libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.3" % Test,
  "org.parboiled" %% "parboiled" % "2.1.8",
  if (`is 2.12+`(scalaVersion.value))
    "org.scala-lang.modules" %% "scala-async" % "0.10.0" % Test
  else
    "org.scala-lang.modules" %% "scala-async" % "0.9.7" % Test,
  "org.scalatest" %% "scalatest" % "3.1.1" % Test)

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

val base = baseDirectory in ThisBuild


def projectWithBaseSrc(project: Project) = project settings (
  unmanagedSourceDirectories in Compile += base.value / "src" / "main" / "scala",
  unmanagedResourceDirectories in Compile += base.value / "src" / "main" / "resources",
  unmanagedSourceDirectories in Test += base.value / "src" / "test" / "scala",
  unmanagedResourceDirectories in Test += base.value / "src" / "test" / "resources")

def mainProject(project: Project) = projectWithBaseSrc(project) settings (
  scalaVersion := "2.13.1",
  crossScalaVersions := Seq("2.11.12", "2.12.11", "2.13.1"),
  dependencies)

def testProject(project: Project) = projectWithBaseSrc(project) settings (
  skip in publish := true,
  scalaVersion := "2.13.1",
  crossScalaVersions := Seq(
    "2.11.6", "2.11.7", "2.11.8", "2.11.9", "2.11.10", "2.11.11", "2.11.12",
    "2.12.0", "2.12.1", "2.12.2", "2.12.3", "2.12.4", "2.12.5", "2.12.6", "2.12.7", "2.12.8", "2.12.9", "2.12.10", "2.12.11",
    "2.13.0", "2.13.1"),
  dependencies,
  dependenciesTest)


lazy val retypecheck = (project in file(".")
  settings (
    skip in publish := true,
    unmanagedSourceDirectories in Compile := Seq.empty,
    unmanagedResourceDirectories in Compile := Seq.empty,
    unmanagedSourceDirectories in Test := Seq.empty,
    unmanagedResourceDirectories in Test := Seq.empty)
  aggregate (main, testMacro, testMacroAnnotation))

lazy val main = (mainProject(project) in file(".main")
  settings (
    normalizedName := "retypecheck",
    excludeFilter in unmanagedSources in Compile := "TyperTester.scala",
    excludeFilter in unmanagedSources in Test := "*"))

lazy val testMacro = (testProject(project) in file(".test-macro"))

lazy val testMacroAnnotation = (testProject(project) in file(".test-macro-annotation")
  settings (macroAnnotation))


test in Test := {
  (test in Test in testMacro).value
  (test in Test in testMacroAnnotation).value
}
