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
val passphrase = System.getenv().get("PGP_PASS") match {
  case x: String => x
  case null => ""
}

lazy val bbc = Project("BBC", file(".")) settings(
  pgpPassphrase := Some(passphrase.toCharArray),
  pgpSecretRing := file("local.secring.gpg"),
  pgpPublicRing := file("local.pubring.gpg"),
  scalaVersion := currentScalaVersion,
  publishMavenStyle := true,
  organization := organizationName,
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.0.4" % "test",
    "org.scala-lang.modules" %% "scala-xml" % "1.0.6",
    "org.yaml" % "snakeyaml" % "1.14",
    "org.activiti" % "activiti-engine" % activitiVersion % "test",
    "org.slf4j" % "slf4j-log4j12" % "1.7.21" % "test",
    "com.h2database" % "h2" % "1.4.192" % "test",
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6"
  ),
  publishTo := {
    if (isSnapshot.value) Some(Opts.resolver.sonatypeSnapshots)
    else Some(Opts.resolver.sonatypeStaging)
  },
  credentials += Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password),
  pomIncludeRepository := { _ => false },
  pomExtra := (
    <url>https://feroshjacob.github.io</url>
      <licenses>
        <license>
          <name>BSD-style</name>
          <url>http://www.opensource.org/licenses/bsd-license.php</url>
          <distribution>repo</distribution>
        </license>
      </licenses>
      <scm>
        <url>git@github.com:recipegrace/BBC.git</url>
        <connection>scm:git:git@github.com:recipegrace/BBC.git</connection>
      </scm>
      <developers>
        <developer>
          <id>feroshjacob</id>
          <name>Ferosh Jacob</name>
          <url>https://feroshjacob.github.io</url>
        </developer>
      </developers>),
  coverallsToken := Some(coverallToken),
  parallelExecution in Test := false,
    resolvers ++= Seq(Resolver.sonatypeRepo("releases")))



