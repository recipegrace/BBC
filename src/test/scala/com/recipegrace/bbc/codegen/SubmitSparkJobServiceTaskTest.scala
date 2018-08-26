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

class SubmitSparkJobServiceTaskTest extends BaseCodeGenTest with SubmitSparkJobServiceTask  {

  val clusterName = "bigbricks-cluster"
  val projectId = "rc-www-search"
  val sparkProps = "a=b"
  val jarURIs = "gs://mosambi/ElectricTemplate-0.0.1.jar"
  val programArgs = Array("--output", "gs://mosambi/testout").map(expr)

  val sparkJobName = "sparkJobName"
  val id = System.currentTimeMillis().toInt

  def submitSparkJob(baseTask: BaseTask) = {
    val sparkJob = SparkJob(sparkJobName, List(
      ArgumentSparkJobConfig(programArgs),
      SparkJobConfigProps(expr(sparkProps)),
      SparkJobConfigClassName(expr(MAINCLASS)),
      SparkJobConfigJarURI(expr(jarURIs))
    ), List())
    createSparkJobServiceTask(id, sparkJob,
      ProgramConfiguration(expr(projectId), expr("us-east1-c"), "ex", None), clusterName,baseTask,Map())

  }



  val expectedValues = Map(
    "-clusterName" -> clusterName,
    "-environment" -> projectId,
    "-region" -> "global",
    "-mainClass" -> MAINCLASS,
    "-properties" -> sparkProps,
    "-jarURIs" -> jarURIs,
    "-programArguments" -> programArgs.map(f=>f.value).mkString(BIGBRICKSDELEMITER))

  test("create sparkJob service task (activit)" ) {

    val xmlContent = submitSparkJob(BaseActivitiServiceTask).asInstanceOf[NodeSeq]
   (xmlContent \\ "serviceTask" \ "@name").text shouldBe s"SparkJob$sparkJobName"
    (xmlContent \\ "serviceTask" \ "@id").text shouldBe "serviceTask" + id.toString

    fieldValueTest(xmlContent, expectedValues)

  }
  test("create sparkJob service task (concourse)" ) {
    val map = submitSparkJob(BaseConcourseDelegateTask).asInstanceOf[Map[String,Any]]
    map("task") shouldBe  ("SparkJobsparkJobName" + id)
    fieldValueTest(map, expectedValues)

  }
}
