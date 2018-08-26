package com.recipegrace.bbc.validations

import com.recipegrace.bbc.workflow.BaseWorkflowTest

class JavaJobValidationsTest  extends BaseWorkflowTest {




  test("variable usage on sbt") {

    assertionErrorOnDeclarationYAMLORTemplate(
      s"""javajob example {
         mainClass = "hello"
         jarLocation= temp
        }
      run example
      """.stripMargin, "variable temp not defined", false)

    assertionErrorOnDeclarationYAMLORTemplate(
      s"""
         javajob example(test) {
             mainClass = "hello"
             jarLocation= temp
        }
      run example
      """.stripMargin, s"definition of javajob:example and usage have different parameters", false)

    assertionErrorOnDeclaration(
      s"""var temp="right"
         javajob example(test,test) {
         mainClass = "hello"
         jarLocation= temp
         jarLocation= temp
        }
    cluster simpleCluster {
                  workers=10
                  image="n1-standard-16"
                  properties="spark:spark.executor.cores=16"
     }
      run example("ss","ss")
      """.stripMargin, "jarLocation have repeated declaration on JavaJob example", true)
    assertionErrorOnDeclaration(
      s"""var temp="right"
         javajob example(test,test) {
         mainClass = "hello"
         mainClass = "hello"
         jarLocation= temp
        }
    cluster simpleCluster {
                  workers=10
                  image="n1-standard-16"
                  properties="spark:spark.executor.cores=16"
     }
      run example("ss","ss")
      """.stripMargin, "mainClass have repeated declaration on JavaJob example", true)
    assertionErrorOnDeclaration(
      s"""javajob example {

               mainClass = "hello"
                  jarLocation= "gs://ow/jol"
                  args = "a", temp
        }

      run example
      """.stripMargin, "variable temp not defined",true)
  }

  test("empty") {




    assertionErrorOnDeclaration(
      s"""javajob example {

               mainClass = ""
                  jarLocation= "gs://ow/jol"
                  args = "a", temp
        }
      """.stripMargin, s"example.mainClass cannot be empty")

    assertionErrorOnDeclaration(
      s"""javajob example {
         |               mainClass = "hola"
         |                  jarLocation= ""
         |                  args = "a", temp
        }
      """.stripMargin, "example.jarLocation cannot be empty")

  }


  test("mandatory") {
    assertionErrorOnDeclaration(
      s"""javajob example {

        }
      """.stripMargin, s"JavaJob example have missing field(s):jarLocation,mainClass")

  }



}
