enablePlugins(GitVersioning)

git.useGitDescribe in ThisBuild := true

name in ThisBuild := "retypecheck"

scalaVersion in ThisBuild := "2.12.8"

organization in ThisBuild := "de.tuda.stg"

licenses in ThisBuild += "Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")

scalacOptions in ThisBuild ++= Seq("-feature", "-deprecation", "-unchecked", "-Xlint")


val dependencies = libraryDependencies ++= Seq(
  scalaOrganization.value % "scala-reflect" % scalaVersion.value % "provided",
  "org.scalamacros" %% "resetallattrs" % "1.0.0")

val dependenciesTest = libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.3" % "test",
  "org.parboiled" %% "parboiled" % "2.1.7",
  "org.scala-lang.modules" %% "scala-async" % "0.9.7" % "test",
  "org.scalatest" %% "scalatest" % "3.0.8" % "test")

val macroparadise = addCompilerPlugin(
  "org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.patch)

val base = baseDirectory in ThisBuild


def projectWithBaseSrc(project: Project) = project settings (
  unmanagedSourceDirectories in Compile += base.value / "src" / "main" / "scala",
  unmanagedResourceDirectories in Compile += base.value / "src" / "main" / "resources",
  unmanagedSourceDirectories in Test += base.value / "src" / "test" / "scala",
  unmanagedResourceDirectories in Test += base.value / "src" / "test" / "resources")

def mainProject(project: Project) = projectWithBaseSrc(project) settings (
  scalaVersion := "2.12.8",
  crossScalaVersions := Seq("2.11.12", "2.12.8"),
  dependencies)

def testProject(project: Project) = projectWithBaseSrc(project) settings (
  skip in publish := true,
  scalaVersion := "2.12.8",
  crossScalaVersions := Seq(
    "2.11.6", "2.11.7", "2.11.8", "2.11.9", "2.11.10", "2.11.11", "2.11.12",
    "2.12.0", "2.12.1", "2.12.2", "2.12.3", "2.12.4", "2.12.5", "2.12.6", "2.12.7", "2.12.8"),
  dependencies,
  dependenciesTest)


lazy val retypecheck = (project in file(".")
  settings (
    skip in publish := true,
    unmanagedSourceDirectories in Compile := Seq.empty,
    unmanagedResourceDirectories in Compile := Seq.empty,
    unmanagedSourceDirectories in Test := Seq.empty,
    unmanagedResourceDirectories in Test := Seq.empty)
  aggregate (main, testScalaMacro, testParadiseMacro))

lazy val main = (mainProject(project) in file(".main")
  settings (
    normalizedName := "retypecheck",
    excludeFilter in unmanagedSources in Compile := "TyperTester.scala",
    excludeFilter in unmanagedSources in Test := "*"))

lazy val testScalaMacro = (testProject(project) in file(".test-scalamacro"))

lazy val testParadiseMacro = (testProject(project) in file(".test-paradisemacro")
  settings (macroparadise))


test in Test := {
  (test in Test in testScalaMacro).value
  (test in Test in testParadiseMacro).value
}
