package com.recipegrace.bbc.validations

import com.recipegrace.bbc.workflow.BaseWorkflowTest

abstract class BaseRepositoryJobTest  extends BaseWorkflowTest {


  def jobName:String
  def validLine:String
  def errorLine1:String
  def emptyLine:String
  def fieldName:String

  test("variable usage on sbt") {

    assertionErrorOnDeclarationYAMLORTemplate(
      s"""$jobName example {
           $validLine
         repository= temp
         container ="something"
         repoBranch="sparkProps"
        }
      run example
      """.stripMargin, "variable temp not defined")

    assertionErrorOnDeclarationYAMLORTemplate(
      s"""
         $jobName example(test) {
         $validLine
         repository= "temp"
         repoBranch="sparkProps"
         container="test"
        }
      run example
      """.stripMargin, s"definition of $jobName:example and usage have different parameters")

    assertionErrorOnDeclaration(
      s"""var temp="right"
         $jobName example(test,test) {
         $validLine
         repository= "temp"
         repoBranch="sparkProps"
         container="test"
        }
    cluster simpleCluster {
                  workers=10
                  image="n1-standard-16"
                  properties="spark:spark.executor.cores=16"
     }
      run example("ss","ss")
      """.stripMargin, "definition variable in example is repeated")
    assertionErrorOnDeclaration(
      s"""var temp="right"
         $jobName example(test,test) {
         $validLine
         repository= "temp"
         repoBranch="sparkProps"
         container="test"
        }

      run example("ss","ss")
      """.stripMargin, "definition variable in example is repeated")
    assertionErrorOnDeclaration(
      s"""$jobName example {
         $errorLine1
         repository= "some"
         repoBranch="sparkProps"
         container="test"

        }

      run example
      """.stripMargin, "variable temp not defined")
  }
  test("empty") {


    assertionErrorOnDeclaration(
      s"""$jobName example {
         $emptyLine
         args= "sdsd"
         container="test"
         repository= "some"
         repoBranch="sparkProps"
        }
      """.stripMargin, s"example.$fieldName cannot be empty")

    assertionErrorOnDeclaration(
      s"""$jobName example {
         $validLine
         args= "sdsd"
         container=""
         repository= "some"
         repoBranch="sparkProps"
        }
      """.stripMargin, "example.container cannot be empty")

  }
  test("repeated") {

    for (content <- List(fieldName, "repoBranch", "repository")) {
      val dsl2 =
        s"""$jobName example {
         $validLine
         args= "sdsd"
         container="something"
         repository= "some"
         repoBranch="sparkProps"
         $content = "$content"
        }
      """.stripMargin
      assertionErrorOnDeclaration(dsl2, content + s" have repeated declaration on $jobName example")
    }
  }



  test("resource generation"){
    getFlowblocks(
      s"""$jobName example {
         $validLine
         args= "sdsd"
         container="sdd"
         repository= "some"
         repoBranch="sparkProps"
        }
      """.stripMargin)("resources") shouldBe List()
    getFlowblocks(
      s"""$jobName example {
         $validLine
         args= "sdsd"
         container="sdd"
         repository= "some"
         repoBranch="sparkProps"
        }
        run example
        run example
      """.stripMargin)("resources").size shouldBe 1

    getInputs(
      s"""$jobName example {
         $validLine
         args= "sdsd"
         container="sdd"
         repository= "some"
         repoBranch="sparkProps"
        }
        run example
        run example
      """.stripMargin) should have size 1
  }

}
