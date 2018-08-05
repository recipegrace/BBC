package com.recipegrace.bbc.validations

import com.recipegrace.bbc.ActivitiMain
import com.recipegrace.bbc.workflow.BaseWorkflowTest

/**
  * Created by Ferosh Jacob on 12/26/16.
  */
class SparkJobValidationsTest extends BaseWorkflowTest {


  test("variable usage on spark") {


    assertionErrorOnDeclaration(
      """sparkjob example {
         mainClass="hola"
         args= temp
         props="sparkProps"
        jarLocation="jarURI"
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
         sparkjob example(test) {
         mainClass="hola"
         args= temp
         props="sparkProps"
        jarLocation="jarURI"
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
         sparkjob example(test,test) {
         mainClass="hola"
         args= temp
         props="sparkProps"
        jarLocation="jarURI"
        }
    cluster simpleCluster {
                  workers=10
                  image="n1-standard-16"
                  properties="spark:spark.executor.cores=16"
     }
      run example("ss","ss") on simpleCluster
      """.stripMargin, "definition variable in example is repeated")
    assertionErrorOnDeclaration(
      """sparkjob example {
         mainClass="hola"
         args= "some"
         props="sparkProps"
        jarLocation="jarURI"
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
      """sparkjob example {
         mainClass=""
         args= "sdsd"
         props="sparkProps"
        jarLocation="jarURI"
        }
      """.stripMargin, "example.mainClass cannot be empty")


    assertionErrorOnDeclaration(
      """sparkjob example {
         mainClass="assd"
         args= "sdsd"
         props="sparkProps"
        jarLocation=""
        }
      """.stripMargin, "example.jarURI cannot be empty")
  }
  test("repeated") {

    for (content <- List("mainClass", "args", "props")) {
      val dsl2 =
        s"""sparkjob example {
         mainClass="assd"
         args= "sdsd"
         props="sparkProps"
         $content = "$content"
        jarLocation="gs://sds"
        }
      """.stripMargin
      assertionErrorOnDeclaration(dsl2, content + " have repeated declaration on SparkJob example")
    }
  }

  test("mandatory") {

    assertionErrorOnDeclaration(
      """sparkjob example {

        }
      """.stripMargin, s"SparkJob example have missing field(s):mainClass,jarLocation")
    assertionErrorOnDeclaration(
      """
       cluster cluster1 {
       workers=2
       image="sdd"
         }
       sparkjob example {
        jarLocation="ssds"
        }
        run example on cluster1
      """.stripMargin, s"SparkJob example have missing field(s):mainClass")

    assertionErrorOnDeclaration(
      """
       cluster cluster1 {
       workers=2
       image="sdd"
         }
       sparkjob example {
        mainClass="mainClass"
        }
        run example on cluster1
      """.stripMargin, s"SparkJob example have missing field(s):jarLocation")
    assertionErrorOnDeclaration(
      """
       cluster cluster1 {
       workers=2
       image="sdd"
         }
       sparkjob example {
        mainClass="mainClass"
        jarLocation="ssds"
        }
        run example
      """.stripMargin, s"Cluster name is not specified for job:example")
  }


  test("invalid link") {
    assertionError(dsl(true, false), "Cluster name is not defined: simpleCluster1")
    assertionError(dsl(false, true), "job:sparkJobName1 is not defined")


  }
}
