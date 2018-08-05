package com.recipegrace.bbc.validations

import com.recipegrace.bbc.workflow.BaseWorkflowTest

/**
  * Created by Ferosh Jacob on 12/26/16.
  */
class ClusterValidationsTest extends BaseWorkflowTest {

  test("mandatory") {

    assertionErrorOnDeclaration(
      """ cluster example1 {
         workers=2
        }
      """.stripMargin, s"Cluster example1 have missing field(s):image")

    assertionErrorOnDeclaration(
      """ cluster example1 {
        }
      """.stripMargin, s"Cluster example1 have missing field(s):image,workers")
  }

  test("empty") {

    assertionErrorOnDeclaration(
      """ cluster example {
         workers=1
         image="simpleimage"
        properties="example properties"
        }
      """.stripMargin, "Numbers of workers should be more than one for cluster example")


    assertionErrorOnDeclaration(
      """ cluster example {
         workers=2
         image=""
        properties="example properties"
        }
              """.stripMargin, "example.image cannot be empty")
  }
  test("repeated") {
    for ((x, y) <- List("workers" -> 2, "image" -> "\"image\"", "properties" -> "\"properties\"")) {

      assertionErrorOnDeclaration(
        s""" cluster example {
         workers=2
         image=""
         $x= $y
        properties="example properties"
        }
              """.stripMargin, x + " have repeated declaration on Cluster example")
    }


  }
  test("non cluster jobs") {
    assertionErrorOnDeclaration(
      """
         cluster simple {
          workers=2
          image="sd"
         properties="example properties"
         }
         pythonjob example {
         mainPyFile="main"
         args= "sdsd"
         container="sdd"
         repository= "some"
         repoBranch="sparkProps"
        }
        run example on simple
      """.stripMargin, "Cluster simple  specified for a python job:example")

    assertionErrorOnDeclaration(
      """
         cluster simple {
          workers=2
          image="sd"
         properties="example properties"
         }
         pythonjob example {
         mainPyFile="main"
         args= "sdsd"
         container="sdd"
         repository= "some"
         repoBranch="sparkProps"
        }
        pipeline pipe {
        run example on simple
        }
        run pipe

     """.stripMargin, "Cluster simple  specified for a python job:example")

    assertionErrorOnDeclaration(
      """
         cluster simple {
          workers=2
          image="sd"
         properties="example properties"
         }
         pythonjob example {
         mainPyFile="main"
         args= "sdsd"
         container="sdd"
         repository= "some"
         repoBranch="sparkProps"
        }
        pipeline pipe {
        run example
        }
        run pipe on simple

     """.stripMargin, "Cluster simple  specified for a pipeline job:pipe")
  }
}
