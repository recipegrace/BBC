package com.recipegrace.bbc.grmr

/**
  * Created by Ferosh Jacob on 11/7/16.
  */

import com.recipegrace.bbc.grmr.BBCStructures._
import com.recipegrace.bbc.grmr.Expressions.{StringExpression, VariableExpression}

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
      case ArgumentSparkJobConfig(x) => x.map(f=> f.value).mkString shouldBe programArguments.mkString
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

      case ArgumentSparkJobConfig(x) => x.map(f=> f.value).mkString shouldBe programArguments.mkString
    }
  }
  test("simple sbtjob  submission test") {
    val mainClass = "sample.co"
    val programArguments = Array("arg1", "arg2")
    val jobName = "Zjob"
    val branch = "develop"
    val container = "recipegrace/hello"
    val repository = "git@recipegrace.com/SS"
    val jobDefintiion =
      s"""sbtjob $jobName {
        mainClass="$mainClass"
        args= ${programArguments.map(f => "\"" + f + "\"").mkString(",")}
        container = "${container}"
         repoBranch = "${branch}"
         repository = "${repository}"
        }
      """.stripMargin


    val job = parseAll(_sbtJobBody, jobDefintiion).get

    job._2.name shouldBe jobName

    job._2.sbtJobConfigs.foreach {
      case SBTJobConfigMainClass(StringExpression(x)) => x shouldBe mainClass
      case RepositoryJobConfigContainer(StringExpression(x)) => x shouldBe container
      case RepositoryJobConfigRepoBranch(StringExpression(x)) => x shouldBe branch
      case RepositoryJobConfigRepository(StringExpression(x)) => x shouldBe repository
      case RepositoryJobConfigArgs(x) => x.map(f=> f.value).mkString shouldBe programArguments.mkString
    }
  }

  test("simple json ") {

    val json1 = """{"a" : "b"}""".stripMargin
    parseAll(_json,json1).get.toString shouldBe json1
    val json2  = """["a", 2, "b"]"""
    parseAll(_json,json2 ).get.toString shouldBe json2
    val json3  =
      """[    "a"    ,
         2   ,
         "b"  ]""".stripMargin
    parseAll(_json,json3 ).get.toString shouldBe json2

    val json4  = """{"somevalue" : ["a", 2, "b"], "some other value" : [23], "else value" : "aesome"}"""
    parseAll(_json,json4 ).get.toString shouldBe json4

  }

  test("json variables") {
    val variableJson =
      s"""var x ={
    "glossary": {
        "title": "example glossary",
		"GlossDiv": {
            "title": "S",
			"GlossList": {
                "GlossEntry": {
                    "ID": "SGML",
					"SortAs": "SGML",
					"GlossTerm": "Standard Generalized Markup Language",
					"Acronym": "SGML",
					"Abbrev": "ISO 8879:1986",
					"GlossDef": {
                        "para": "A meta-markup language, used to create markup languages such as DocBook.",
						"GlossSeeAlso": ["GML", "XML"]
                    },
					"GlossSee": "markup"
                }
            }
        }
    }
}""".stripMargin
    val variableDeclaration:VariableDeclaration = parseAll(_variableDeclarations,variableJson).get._2.variables.head
    variableDeclaration.name shouldBe "x"
    s"${variableDeclaration.value}" shouldBe """{"glossary" : {"title" : "example glossary", "GlossDiv" : {"title" : "S", "GlossList" : {"GlossEntry" : {"GlossDef" : {"para" : "A meta-markup language, used to create markup languages such as DocBook.", "GlossSeeAlso" : ["GML", "XML"]}, "GlossTerm" : "Standard Generalized Markup Language", "GlossSee" : "markup", "Abbrev" : "ISO 8879:1986", "Acronym" : "SGML", "SortAs" : "SGML", "ID" : "SGML"}}}}}"""




  }
  test("simple pythonjob  submission test") {
    val mainPyFile = "sample.py"
    val programArguments = Array("arg1", "arg2")
    val jobName = "Zjob"
    val branch = "develop"
    val container = "recipegrace/hello"
    val repository = "git@recipegrace.com/SS"
    val jobDefintiion =
      s"""pythonjob $jobName {
        mainPyFile="$mainPyFile"
        args= ${programArguments.map(f => "\"" + f + "\"").mkString(",")}
        container = "${container}"
         repoBranch = "${branch}"
         repository = "${repository}"
        }
      """.stripMargin


    val job = parseAll(_pyJobBody, jobDefintiion).get

    job._2.name shouldBe jobName

    job._2.pyJobConfigs.foreach {
      case PyJobConfigMainPyFile(StringExpression(x)) => x shouldBe mainPyFile
      case RepositoryJobConfigContainer(StringExpression(x)) => x shouldBe container
      case RepositoryJobConfigRepoBranch(StringExpression(x)) => x shouldBe branch
      case RepositoryJobConfigRepository(StringExpression(x)) => x shouldBe repository
      case RepositoryJobConfigArgs(x) => x.map(f=> f.value).mkString shouldBe programArguments.mkString
    }
  }

  test("simple webservice (post) submission test") {
    val url = "sample.py"
    val json = """{"a" : "b"}"""
    val jobName = "Zjob"
    val jobDefintiion =
      s"""webservicepost $jobName {
        url="$url"
        json = $json

        }
      """.stripMargin


    val job = parseAll(_webservicePostBody, jobDefintiion).get

    job._2.name shouldBe jobName

    job._2.webserviceJobConfigs.foreach {
      case WebservicePostJobDataConfig(x) => s"$x" shouldBe json
      case WebserviceJobURLConfig(StringExpression(x)) => x shouldBe url

    }
  }
  test("simple webservice (get) submission test") {
    val url = "sample.py"
    val programArguments = Array("arg1", "arg2")
    val jobName = "Zjob"
    val jobDefintiion =
      s"""webserviceget $jobName {
        url="$url"
       args= ${programArguments.map(f => "\"" + f + "\"").mkString(",")}

        }
      """.stripMargin


    val job = parseAll(_webserviceGetBody, jobDefintiion).get

    job._2.name shouldBe jobName

    job._2.webserviceJobConfigs.foreach {
      case WebserviceGetJobConfigArgs(x)=> x.map(f=> f.value).mkString shouldBe programArguments.mkString
      case WebserviceJobURLConfig(StringExpression(x)) => x shouldBe url

    }
  }

  test("simple javajob test") {
    val mainClass = "sample.py"
    val programArguments = Array("arg1", "arg2")
    val jobName = "Zjob"
    val jarLocation = "someplace"
    val properties = "properties"
    val jobDefintiion =
      s"""javajob $jobName {
        mainClass="$mainClass"
       args= ${programArguments.map(f => "\"" + f + "\"").mkString(",")}
       jarLocation = $jarLocation
         props = $properties
        }
      """.stripMargin


    val job = parseAll(_javaJobBody, jobDefintiion).get

    job._2.name shouldBe jobName

    job._2.javaJobConfigs.foreach {
      case ArgumentJavaJobConfig(x)=> x.map(f=> f.value).mkString shouldBe programArguments.mkString
      case MainClassJavaJobConfig(StringExpression(x)) => x shouldBe mainClass
      case JarLocationJavaJobConfig(VariableExpression(x)) => x shouldBe jarLocation
      case PropertiesJavaJobConfig  (VariableExpression(x)) => x shouldBe properties

    }
  }
}
