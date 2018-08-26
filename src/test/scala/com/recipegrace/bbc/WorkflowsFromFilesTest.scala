package com.recipegrace.bbc

import java.io.{FileReader, FileWriter}

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


  private def printOutConcourseAndComposePipeLines(fileName: String,compose:Boolean=true, concourse:Boolean=false) = {

    val main =if(concourse) ConcourseMain else ComposerMain
    val content = main.generateProcess(new FileReader("files/" + fileName))
    //val content = main.generateProcess(new FileReader("/tmp/recall-relevance-slow.bbc"))

    main.errorMessage shouldBe ""
    main.generateFlows.clusterStore.running shouldBe List()
    println(content.get)
    val writer = new FileWriter(".tests/output.txt")
    writer.append(content.get)
    writer.close()
  }

  test("main file test (concourse)") {

    val fileName = "simplejob.bbc"
    printOutConcourseAndComposePipeLines(fileName)

  }

  test("main file test (compose)") {

    val fileName = "javajob.bbc"
    printOutConcourseAndComposePipeLines(fileName,compose=true,concourse = false)

  }
  test("main file test (composer)") {

    val fileName = "basicjob.bbc"
    printOutConcourseAndComposePipeLines(fileName, true)

  }

  test("python and pyspark (concourse)") {

    val fileName = "sparkpythonjob.bbc"
    printOutConcourseAndComposePipeLines(fileName)

  }


  test("java (concourse)") {

    val fileName = "sbtjob.bbc"
    printOutConcourseAndComposePipeLines(fileName)

  }

  test("json (concourse)") {

    val fileName = "jsonexample.bbc"
    printOutConcourseAndComposePipeLines(fileName)

  }

  test("main file test (concourse/composer) pyspark") {

    val fileName1 = "pysparkjob.bbc"
    printOutConcourseAndComposePipeLines(fileName1)

  }
  test("main file test (concourse) pyspark 2") {

    val fileName2 = "sparkandpyspark.bbc"
    printOutConcourseAndComposePipeLines(fileName2)

  }
  test("main file test (concourse) spark, pyspark, python") {

    val fileName2 = "sparkpysparkandpython.bbc"
    printOutConcourseAndComposePipeLines(fileName2)

  }
  test("main file test (concourse) python") {

    val fileName1 = "pythonjob.bbc"
    printOutConcourseAndComposePipeLines(fileName1)

  }
  test("main file test (concourse) pipeline") {

    val fileName1 = "pipelinejob.bbc"
    printOutConcourseAndComposePipeLines(fileName1)

  }
  test("main file test (concourse) artifactory") {

    val fileName = "simpleartifactoryjob.bbc"
    printOutConcourseAndComposePipeLines(fileName)


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
