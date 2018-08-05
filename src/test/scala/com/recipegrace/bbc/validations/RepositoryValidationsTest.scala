package com.recipegrace.bbc.validations

import com.recipegrace.bbc.workflow.BaseWorkflowTest

/**
  * Created by Ferosh Jacob on 12/24/16.
  */
class RepositoryValidationsTest extends BaseWorkflowTest with BaseValidations {

  val goodURL = "http://www.google.com"

  test("nexus test") {
    assertionErrorOnDeclaration(
      s"""
        cloudWorkspace="gs://example/elt"
        nexus example {
        repository="snapshots"
        username="nexus"
        password="nexus"
        }
      """.stripMargin, s"Nexus example have missing field(s):url")

    assertionErrorOnDeclaration(
      s"""
        cloudWorkspace="gs://example/elt"
        nexus example {
        url="$goodURL"
        username="nexus"
        password="nexus"
        }
      """.stripMargin, s"Nexus example have missing field(s):repository")


    assertionErrorOnDeclaration(
      s"""
         cloudWorkspace="gs://example/elt"
        nexus example {
        username="nexus"
        password="nexus"
        }
      """.stripMargin, s"Nexus example have missing field(s):url,repository")
  }
  test("url test") {
    false shouldBe isValidURL(expr("hello"))
    true shouldBe isValidURL(expr("http://www.google.com"))
  }
  test("empty") {



    for(repository <- List("nexus", "artifactory")) {


      assertionErrorOnDeclaration(
        s"""
        cloudWorkspace="gs://example/elt"
        $repository  example {
        url="$goodURL"
        repository=""
        ${ if(repository=="nexus") {
          """username = "nexus"
          password = "nexus""""
        } else ""
        }

        }
      """.stripMargin, "example.repository cannot be empty")
      assertionErrorOnDeclaration(
        s"""
        $repository example {
        url="$goodURL"
        repository="snapshots"
        ${ if(repository=="nexus") {
          """username = "nexus"
          password = "nexus""""
        } else ""
        }
        }
      """.stripMargin, "cloudWorkspace cannot be empty when you use nexus")


      assertionErrorOnDeclaration(
        s"""
        cloudWorkspace="gs://example/elt"
         $repository  example {
        url=""
        repository="snapshots"
        ${ if(repository=="nexus") {
          """username = "nexus"
          password = "nexus""""
        } else ""
        }
        }
      """.stripMargin, s"example.url is not a valid URL")



      assertionErrorOnDeclaration(
        s"""
        cloudWorkspace="gs://example/elt"
        $repository example {
        url="http://www.google.com"
        repository="nexus"
        }
        sparkjob test1 {
        mainClass="main"
        jarLocation= %"" % "dee" % "0.0.1" from example
        }
        cluster jobOnCluster {
        workers=2
        image ="sddd"
        }
        run test1 on jobOnCluster
      """.stripMargin, s"test1.jarLocation.organization cannot be empty")
    }


  }
  test("validate links") {
    for(repository <- List("nexus", "artifactory")) {
      assertionErrorOnDeclaration(
        s"""
        cloudWorkspace="gs://example/elt"
        $repository example {
        url="http://www.google.com"
        repository="nexus"
        }
        $repository example {
        url="http://www.google.com"
        repository="nexus"
        }
      """.stripMargin, s"Nexus/Artifactory has repeated declarations with the same name: example")

      assertionErrorOnDeclaration(
        s"""
        cloudWorkspace="gs://example/elt"
        $repository example {
        url="http://www.google.com"
        repository="anyname"
        }
        sparkjob test1 {
        mainClass="main"
        jarLocation= % "com" % "dee" % "0.0.1" from example1
        }
        cluster jobOnCluster {
        workers=2
        image ="sddd"
        }
        run test1 on jobOnCluster
      """.stripMargin, s"Nexus/Artifactory example1 not defined on SparkJob test1")

    }

}

  test("repeated nexus") {

    for ((x, y) <- List("url" -> "http://sds/com", "repository" -> "repo", "username" -> "username", "password" -> "password")) {
      assertionErrorOnDeclaration(
        s"""
         cloudWorkspace="gs://example/elt"
        nexus example {
        url="http://www.google.com"
        $x="$y"
         repository="nexus"
        username="nexus"
        password="nexus"
        }
      """.stripMargin, x + " have repeated declaration on Nexus Repo example")
    }
  }
  test("repeated artifactory") {

    for ((x, y) <- List("url" -> "http://sds/com", "repository" -> "repo")) {
      assertionErrorOnDeclaration(
        s"""
         cloudWorkspace="gs://example/elt"
        artifactory example {
        url="http://www.google.com"
        $x="$y"
         repository="artifactory"
        }
      """.stripMargin, x + " have repeated declaration on Artifactory Repo example")
    }
  }
  test("mandatory") {

    for(repository <- List("nexus" -> "Nexus", "artifactory" ->"Artifactory")) {
      assertionErrorOnDeclaration(
        s"""
         cloudWorkspace="gs://example/elt"
        ${repository._1} example {
        }
      """.stripMargin, s"${repository._2} example have missing field(s):url,repository")
      assertionErrorOnDeclaration(
        s"""
         cloudWorkspace="gs://example/elt"
         ${repository._1} example {
        repository="test"
        }
      """.stripMargin, s"${repository._2} example have missing field(s):url")
    }
  }
}
