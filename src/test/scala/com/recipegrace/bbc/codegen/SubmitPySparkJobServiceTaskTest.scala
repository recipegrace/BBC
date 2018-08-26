package com.recipegrace.bbc.codegen

/**
  * Created by Ferosh Jacob on 11/8/16.
  */

import com.recipegrace.bbc.activiti.BaseActivitiServiceTask
import com.recipegrace.bbc.concourse.BaseConcourseDelegateTask
import com.recipegrace.bbc.grmr.BBCStructures._
import com.recipegrace.bbc.concourse.BaseConcourseDelegateTask
import com.recipegrace.bbc.workflow.ProgramConfiguration

import scala.xml.NodeSeq

class SubmitPySparkJobServiceTaskTest extends BaseCodeGenTest with SubmitPySparkJobServiceTask  {

  val clusterName = "bigbricks-cluster"
  val projectId = "rc-www-search"
  val sparkProps = "a=b"
  val morePyFiles = Array("gs://mosambi/ElectricTemplate-0.0.1.jar").map(expr)
  val programArgs = Array("--output", "gs://mosambi/testout").map(expr)

  val sparkJobName = "pySparkJobName"
  val id = System.currentTimeMillis().toInt

  def submitSparkJob(baseTask: BaseTask) = {
    val pySparkJob = PySparkJob(sparkJobName, List(
      ArgumentSparkJobConfig(programArgs),
      PySparkJobConfigProps(expr(sparkProps)),
      PySparkJobConfigMainPyFile(expr(MAINCLASS)),
      PySparkJobConfigOtherPyFiles(morePyFiles)
    ), List())
    createPySparkJobServiceTask(id, pySparkJob,
      ProgramConfiguration(expr(projectId), expr("us-east1-c"), "ex", None), clusterName,baseTask,Map())

  }



  val expectedValues = Map(
    "-clusterName" -> clusterName,
    "-environment" -> projectId,
    "-region" -> "global",
    "-mainPyFile" -> MAINCLASS,
    "-properties" -> sparkProps,
    "-otherPyFiles" -> morePyFiles.map(f=>f.value).mkString(","),
    "-programArguments" -> programArgs.map(f=>f.value).mkString(BIGBRICKSDELEMITER))

  test("create sparkJob service task (activit)" ) {

    val xmlContent = submitSparkJob(BaseActivitiServiceTask).asInstanceOf[NodeSeq]
   (xmlContent \\ "serviceTask" \ "@name").text shouldBe s"PySparkJob$sparkJobName"
    (xmlContent \\ "serviceTask" \ "@id").text shouldBe "serviceTask" + id.toString

    fieldValueTest(xmlContent, expectedValues)

  }
  test("create sparkJob service task (concourse)" ) {
    val map = submitSparkJob(BaseConcourseDelegateTask).asInstanceOf[Map[String,Any]]
    map("task") shouldBe  ("PySparkJob"+sparkJobName + id)
    fieldValueTest(map, expectedValues)

  }
}
