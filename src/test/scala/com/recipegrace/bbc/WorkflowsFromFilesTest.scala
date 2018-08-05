package com.recipegrace.bbc

import java.io.FileReader

import com.recipegrace.bbc.workflow.BaseWorkflowTest

/**
  * Created by Ferosh Jacob on 11/25/16.
  */
class WorkflowsFromFilesTest extends BaseWorkflowTest {

  test("main file test (activiti)") {

    val fileName = "simplejob.bbc"
    val content = ActivitiMain.generateProcess(new FileReader("files/" + fileName))
    ActivitiMain.errorMessage shouldBe ""
    deployToCheckTheXMLisValid(content.get)

  }






  private def printOutConcoursePipeLine(fileName: String) = {
    val content = ConcourseMain.generateProcess(new FileReader("files/" + fileName))
    ConcourseMain.errorMessage shouldBe ""
    ConcourseMain.generateFlows.clusterStore.running shouldBe List()
    println(content.get)
  }

  test("main file test (concourse)") {

    val fileName = "simplejob.bbc"
    printOutConcoursePipeLine(fileName)

  }

  test("python and pyspark (concourse)") {

    val fileName = "sparkpythonjob.bbc"
    printOutConcoursePipeLine(fileName)

  }


  test("java (concourse)") {

    val fileName = "javajob.bbc"
    printOutConcoursePipeLine(fileName)

  }

  test("main file test (concourse) pyspark") {

    val fileName1 = "pysparkjob.bbc"
    printOutConcoursePipeLine(fileName1)

  }
  test("main file test (concourse) pyspark 2") {

    val fileName2 = "sparkandpyspark.bbc"
    printOutConcoursePipeLine(fileName2)

  }
  test("main file test (concourse) spark, pyspark, python") {

    val fileName2 = "sparkpysparkandpython.bbc"
    printOutConcoursePipeLine(fileName2)

  }
  test("main file test (concourse) python") {

    val fileName1 = "pythonjob.bbc"
    printOutConcoursePipeLine(fileName1)

  }
  test("main file test (concourse) pipeline") {

    val fileName1 = "pipelinejob.bbc"
    printOutConcoursePipeLine(fileName1)

  }
  test("main file test (concourse) artifactory") {

    val fileName = "simpleartifactoryjob.bbc"
    printOutConcoursePipeLine(fileName)


  }
  test("multiple nexus test") {
    val reader= new FileReader("files/multiplejobs.bbc")
   // val yaml =generateYAMLObject(new StringReader(reader))("jobs").asInstanceOf[List[Map[String,Any]]].size shouldBe 15
     println(ConcourseMain.generateProcess(reader).get)
  }

  test("more examples nexus test") {
    val reader= new FileReader("files/moreexamples.bbc")
    // val yaml =generateYAMLObject(new StringReader(reader))("jobs").asInstanceOf[List[Map[String,Any]]].size shouldBe 15
    println(ConcourseMain.generateProcess(reader).get)
  }
  test("more examples nexus fake test") {
    val reader= new FileReader("files/fakeexample.bbc")
    // val yaml =generateYAMLObject(new StringReader(reader))("jobs").asInstanceOf[List[Map[String,Any]]].size shouldBe 15
    println(ConcourseMain.generateProcess(reader).get)
  }
}
