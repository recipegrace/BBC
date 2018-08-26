import com.typesafe.sbt.SbtPgp.autoImportImpl._
import org.scoverage.coveralls.Imports.CoverallsKeys._
import sbt.Keys._
import sbt._
val currentScalaVersion = "2.12.2"
val organizationName = "com.recipegrace"
val activitiVersion = "5.17.0"
val username = System.getenv().get("SONATYPE_USERNAME")
val password = System.getenv().get("SONATYPE_PASSWORD")
val coverallToken = System.getenv().get("COVERALL_TOKEN")
val passphrase = System.getenv().get("PGP_PASSPHRASE") match {
  case x: String => x
  case null => ""
}

lazy val bbc = Project("BBC", file(".")) settings(
  scalaVersion := currentScalaVersion,
  coverallsServiceName := Some("travis-pro"),
  publishMavenStyle := true,
  pgpPassphrase := Some( passphrase.toCharArray),
  pgpSecretRing := file("secring.gpg"),
  pgpPublicRing := file("pubring.gpg"),
  organization := organizationName,
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.0.4" % "test",
    "org.scala-lang.modules" %% "scala-xml" % "1.0.6",
    "org.yaml" % "snakeyaml" % "1.14",
    "org.scalatra.scalate" %% "scalate-core" % "1.9.0",
    "org.activiti" % "activiti-engine" % activitiVersion % "test",
    "org.slf4j" % "slf4j-log4j12" % "1.7.21" % "test",
    "com.h2database" % "h2" % "1.4.192" % "test",
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6"
  ),
  credentials += Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password),
  pomIncludeRepository := { _ => false },
  licenses := Seq("Apache 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
  publishArtifact in Test := false,
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases"  at nexus + "service/local/staging/deploy/maven2")
  },
  homepage := Some(url("https://github.com/recipegrace/BBC")),
    scmInfo := Some(
    ScmInfo(
      url("https://github.com/recipegrace/BBC"),
      "scm:git:git@github.com:recipegrace/BBC.git"
    )
    ),
    developers := List(
    Developer("fjacob", "Ferosh Jacob", "feroshjacob@gmail.com", url("https://feroshjacob.github.io"))
    ),
  coverallsToken := Some(coverallToken),
  parallelExecution in Test := false,
    resolvers ++= Seq(Resolver.sonatypeRepo("releases")))



