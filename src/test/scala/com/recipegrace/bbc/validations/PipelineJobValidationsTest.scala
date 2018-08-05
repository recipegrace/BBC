package com.recipegrace.bbc.validations

import com.recipegrace.bbc.workflow.BaseWorkflowTest

/**
  * Created by Ferosh Jacob on 12/26/16.
  */
class PipelineJobValidationsTest extends BaseWorkflowTest {


  test("variable usage on pipeline job") {


    assertionErrorOnDeclarationYAML(
      """var letuscheck="23"
         pythonjob example {
         mainPyFile="hola"
         repository= temp
         repoBranch="sparkProps"
         container="test"
        }
        pipeline letuscheck {
          run example
        }
        run letuscheck
      """.stripMargin, "Declaration has repeated declarations with the same name: letuscheck")


    assertionErrorOnDeclarationYAML(
      """var letuscheck="23"
       pysparkjob example(test) {
         mainPyFile="sd"
         otherPyFiles="ssds"
         }
        pipeline letuscheck(two) {
          run example (two)
        }
        run example("first")
      """.stripMargin, "Declaration has repeated declarations with the same name: letuscheck")


    assertionErrorOnDeclarationYAML(
      """
       pysparkjob example(test) {
         mainPyFile=test
         otherPyFiles="ssds"
         }
        pipeline letuscheck(two) {
          run example (two)
        }
        run letuscheck("first")
      """.stripMargin, "Cluster name is not specified for job:example")



  assertionErrorOnDeclaration(
    """
       pysparkjob example(test) {
         mainPyFile=test
         otherPyFiles="ssds"
         }
        pipeline letuscheck(two) {
          run example (two)
          run letuscheck(two)
        }
        run letuscheck("first")
      """.stripMargin, "Pipeline (letuscheck)  inside pipeline (letuscheck) not allowed!")
}


  test("cluster job") {
 newClustersCreated(
      """
               cluster cluster1 {
               workers=2
               image="sdd"
                 }
               sparkjob example1 {
                mainClass="mainClass"
                jarLocation="ssds"
                }
                    pysparkjob example2 {
          mainPyFile="test"
          otherPyFiles="ssds"
          }
        pipeline letuscheck1 {
          run example1 on cluster1
          run example2 on cluster1
        }
        pipeline letuscheck2 {
          run example2 on cluster1
          run example1 on cluster1
        }
                run letuscheck1
                run letuscheck2
      """) shouldBe 1


    newClustersCreated(
      """
               cluster cluster1 {
               workers=2
               image="sdd"
                 }
               cluster cluster2 {
               workers=2
               image="sdd"
                 }
               sparkjob example1 {
                mainClass="mainClass"
                jarLocation="ssds"
                }
                    pysparkjob example2 {
          mainPyFile="test"
          otherPyFiles="ssds"
          }
        pipeline letuscheck1 {
          run example1 on cluster1
          run example2 on cluster2
        }
        pipeline letuscheck2 {
          run example2 on cluster2
          run example1 on cluster1
        }
                run letuscheck1
                run letuscheck2
      """) shouldBe 2


  }

  test("debug help"){
    newClustersCreated(
      """
               cluster cluster1 {
               workers=2
               image="sdd"
                 }
               sparkjob example1 {
                mainClass="mainClass"
                jarLocation="ssds"
                }
                    pysparkjob example2 {
          mainPyFile="test"
          otherPyFiles="ssds"
          }
        pipeline letuscheck1 {
          run example1 on cluster1
          run example2 on cluster1
        }
        pipeline letuscheck2 {
          run example2 on cluster1
          run example1 on cluster1
        }
                run example1 on cluster1
                run letuscheck1
                run letuscheck2
                run example1 on cluster1
                run example2 on cluster1
      """) shouldBe 1

  }
}
