package com.recipegrace.bbc.validations

import com.recipegrace.bbc.{ActivitiMain, ConcourseMain}
import com.recipegrace.bbc.workflow.BaseWorkflowTest

/**
  * Created by Ferosh Jacob on 12/26/16.
  */
class PySparkJobValidationsTest extends BaseWorkflowTest {


  test("variable usage on spark") {


    assertionErrorOnDeclaration(
      """pysparkjob example {
         mainPyFile="hola"
         args= temp
         props="sparkProps"
         otherPyFiles="jarURI"
        }
    cluster simpleCluster {
                  workers=10
                  image="n1-standard-16"
                  properties="spark:spark.executor.cores=16"
     }
      run example on simpleCluster
      """.stripMargin, "variable temp not defined")

    assertionErrorOnDeclaration(
      """var temp="right"
         pysparkjob example(test) {
         mainPyFile="hola"
         args= temp
         props="sparkProps"
        otherPyFiles="jarURI"
        }
    cluster simpleCluster {
                  workers=10
                  image="n1-standard-16"
                  properties="spark:spark.executor.cores=16"
     }
      run example() on simpleCluster
      """.stripMargin, "definition of spark job:example and usage have different parameters")

    assertionErrorOnDeclaration(
      """var temp="right"
         pysparkjob example(test,test) {
         mainPyFile="hola"
         args= temp
         props="sparkProps"
        otherPyFiles="jarURI"
        }
    cluster simpleCluster {
                  workers=10
                  image="n1-standard-16"
                  properties="spark:spark.executor.cores=16"
     }
      run example("ss","ss") on simpleCluster
      """.stripMargin, "definition variable in example is repeated")
    assertionErrorOnDeclaration(
      """pysparkjob example {
         mainPyFile="hola"
         args= "some"
         props="sparkProps"
        otherPyFiles="jarURI"
        }
    cluster simpleCluster {
                  workers=10
                  image=temp
                  properties="spark:spark.executor.cores=16"
     }
      run example on simpleCluster
      """.stripMargin, "variable temp not defined")
  }
    test("empty") {


    assertionErrorOnDeclaration(
      """pysparkjob example {
         mainPyFile=""
         args= "sdsd"
         props="sparkProps"
        otherPyFiles="jarURI"
        }
      """.stripMargin, "example.mainPyFile cannot be empty")


  }
  test("repeated") {

    for (content <- List("mainPyFile", "args", "props")) {
      val dsl2 =
        s"""pysparkjob example {
         mainPyFile="assd"
         args= "sdsd"
         props="sparkProps"
         $content = "$content"
        otherPyFiles="gs://sds"
        }
      """.stripMargin
      assertionErrorOnDeclaration(dsl2, content + " have repeated declaration on PySparkJob example")
    }
  }

  test("mandatory") {

    assertionErrorOnDeclaration(
      """pysparkjob example {

        }
      """.stripMargin, s"PySparkJob example have missing field(s):mainPyFile")
    assertionErrorOnDeclaration(
      """
       cluster cluster1 {
       workers=2
       image="sdd"
         }
       pysparkjob example {
        otherPyFiles="ssds"
        }
        run example on cluster1
      """.stripMargin, s"PySparkJob example have missing field(s):mainPyFile")

    assertionErrorOnDeclaration(
      """
       cluster cluster1 {
       workers=2
       image="sdd"
         }
       pysparkjob example {
        mainPyFile="sd"
        otherPyFiles="ssds"
        }
        run example
      """.stripMargin, s"Cluster name is not specified for job:example")
  }


  test("invalid link") {
      assertionError(dsl(true, false), "Cluster name is not defined: simpleCluster1")
    assertionError(dsl(false, true), "job:sparkJobName1 is not defined")


  }
}
