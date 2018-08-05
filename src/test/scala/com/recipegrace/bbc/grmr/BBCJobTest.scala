package com.recipegrace.bbc.grmr

/**
  * Created by Ferosh Jacob on 11/7/16.
  */

import com.recipegrace.bbc.grmr.BBCStructures._
import com.recipegrace.bbc.grmr.Expressions.StringExpression

class BBCJobTest extends BaseBBCGrammarTest  {


  test("simple repo test") {
    val url = "https://repository.sonatype.org/service/local/artifact/maven/redirect"
    val reponame = "central-proxy"
    val nexusName = "nexusCentralProxy"
    val repoDefinition =
      s"""
        nexus $nexusName {
        url="$url"
        repository="$reponame"

        }
      """.stripMargin
    val content = parseAll(_nexusBody, repoDefinition)
    println(content)
    val nexus = content.get
    nexusName shouldBe nexus._2.name

  }

  test("jar uri from nexus") {
    val location = "gs://sdsdsds/sdsd"
    val code = s""""$location""""
    location shouldBe parseAll(_jarLocation, code).get.asInstanceOf[SparkJobConfigJarURI].uri.asInstanceOf[StringExpression].text

    val org = "com.recipegrace"
    val artifact = "databricks-core"
    val version = "1.0.0"
    val nexusJar = s""" % "$org" %  "$artifact" % "$version" from nexus """

    println(nexusJar)

    println(parseAll(_jarLocation, nexusJar))
    parseAll(_jarLocation, nexusJar).get match {
      case p@RepositoryJarURI(StringExpression(x), StringExpression(y), StringExpression(z), a) => {
        org shouldBe x
        artifact shouldBe y
        version shouldBe z
        "nexus" shouldBe a
      }
      case x => println(x)
    }

  }
  test("simple job submission test") {
    val className = "com.x.y.Z"
    val sparkProps = "-x.ua  y.ub"
    val programArguments = Array("arg1", "arg2")
    val jarURI = "jar link"
    val jobName = "Zjob"
    val jobDefintiion =
      s"""sparkjob $jobName {
        mainClass="$className"
        args= ${programArguments.map(f => "\"" + f + "\"").mkString(",")}
        props="$sparkProps"
        jarLocation="$jarURI"
        }
      """.stripMargin


    val job = parseAll(_sparkJobBody, jobDefintiion).get

    job._2.name shouldBe jobName

    job._2.sparkJobConfigs.foreach {
      case SparkJobConfigClassName(StringExpression(x)) => x shouldBe className
      case SparkJobConfigProps(StringExpression(x)) => x shouldBe sparkProps
      case SparkJobConfigJarURI(StringExpression(x)) => x shouldBe jarURI
      case SparkJobConfigArgs(x) => x.map(f=> f.value).mkString shouldBe programArguments.mkString
    }
  }

  test("simple pysparkjob submission test") {
    val mainPyFile = "samplel.py"
    val sparkProps = "-x.ua  y.ub"
    val programArguments = Array("arg1", "arg2")
    val otherPyFiles = Array("sample1.py","sample2.py")
    val jobName = "Zjob"
    val jobDefintiion =
      s"""pysparkjob $jobName {
        mainPyFile="$mainPyFile"
        args= ${programArguments.map(f => "\"" + f + "\"").mkString(",")}
        props="$sparkProps"
        otherPyFiles=${otherPyFiles.map(f => "\"" + f + "\"").mkString(",")}
        }
      """.stripMargin


    val job = parseAll(_pySparkJobBody, jobDefintiion).get

    job._2.name shouldBe jobName

    job._2.pySparkJobConfigs.foreach {
      case PySparkJobConfigMainPyFile(StringExpression(x)) => x shouldBe mainPyFile
      case PySparkJobConfigProps(StringExpression(x)) => x shouldBe sparkProps
      case PySparkJobConfigOtherPyFiles(x) =>x.map(f=> f.value).mkString shouldBe otherPyFiles.mkString

      case PySparkJobConfigArgs(x) => x.map(f=> f.value).mkString shouldBe programArguments.mkString
    }
  }
}
