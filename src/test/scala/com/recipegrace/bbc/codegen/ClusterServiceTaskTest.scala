package com.recipegrace.bbc.codegen

/**
  * Created by Ferosh Jacob on 11/8/16.
  */

import com.recipegrace.bbc.activiti.BaseActivitiServiceTask
import com.recipegrace.bbc.concourse.BaseConcourseDelegateTask
import com.recipegrace.bbc.grmr.BBCStructures._
import com.recipegrace.bbc.concourse.BaseConcourseDelegateTask

import scala.xml.NodeSeq

class ClusterServiceTaskTest extends BaseCodeGenTest with ClusterServiceTask {

  val id = System.currentTimeMillis().toInt
  val workers = 0
  val envName = "qa"
  val clusterName = "datacluster"
  val initScript = "initScript"
  val timeOut = "timeOut"
  val version = "1.0"

  def generateCreateClusterCode(baseTask: BaseTask) = {
    val cluster = Cluster(id, clusterName, List(ClusterConfigImage(expr(IMAGE)), ClusterConfigProperties(List(expr(PROPERTIES))),
      ClusterConfigWorkers(expr(workers))))
    createClusterServiceTask(cluster, expr(envName), expr(ZONE),baseTask)
  }

  def generateCreateClusterCodeWithInitScript(baseTask: BaseTask) = {
    val cluster = Cluster(id, clusterName, List(ClusterConfigImage(expr(IMAGE)), ClusterConfigProperties(List(expr(PROPERTIES))),
      ClusterConfigWorkers(expr(workers)), ClusterConfigInitialScript(expr(initScript)), ClusterConfigInitialScriptTimeOut(expr(timeOut))))
    createClusterServiceTask(cluster, expr(envName), expr(ZONE),baseTask)
  }

  def generateCreateClusterCodeWithInitScriptAndVersion(baseTask: BaseTask) = {
    val cluster = Cluster(id, clusterName, List(ClusterConfigImage(expr(IMAGE)), ClusterConfigProperties(List(expr(PROPERTIES))),
      ClusterConfigWorkers(expr(workers)), ClusterConfigInitialScript(expr(initScript)), ClusterConfigInitialScriptTimeOut(expr(timeOut)), ClusterConfigVersion(expr(version))))
    createClusterServiceTask(cluster, expr(envName), expr(ZONE),baseTask)
  }
  def generateDeleteClusterCode(baseTask: BaseTask) = {
    deleteClusterServiceTask(List(clusterName), expr(envName), id,baseTask)
  }
  val createExpected = Map(
    "-network" -> "internal",
    "-environment" -> envName,
    "-numOfWorkers" -> workers.toString,
    "-image" -> IMAGE,
    "-zone" -> ZONE,
    "-tags" -> "dataproc",
    "-properties" -> PROPERTIES
    )
  val createExpectedWithInit = createExpected ++ Map("-initialScript"-> initScript, "-initialScriptTimeOut"-> timeOut)
  val createExpectedWithInitWithVersion = createExpected ++ Map("-initialScript"-> initScript, "-initialScriptTimeOut"-> timeOut, "-version" -> version)

  val deleteExpected = Map(
    "-clusterName" -> clusterName,
    "-environment" -> envName,
    "-region" -> "global"
  )

  test("create cluster service (activiti)") {

    val xmlContent = generateCreateClusterCode(BaseActivitiServiceTask).asInstanceOf[NodeSeq]

    (xmlContent \\ "serviceTask" \ "@name").text shouldBe "CreateClusterTask" + id
    (xmlContent \\ "serviceTask" \ "@id").text shouldBe "serviceTask" + id.toString



    fieldValueTest(xmlContent, createExpected)

  }

  test("create cluster service (concourse)") {

    val map = generateCreateClusterCode(BaseConcourseDelegateTask).asInstanceOf[Map[String,Any]]
    map("task") shouldBe  ("CreateClusterTask" + id+id)


    fieldValueTest(map, createExpected)

  }
  test("create cluster service (concourse) initial script") {

    val map = generateCreateClusterCodeWithInitScript(BaseConcourseDelegateTask).asInstanceOf[Map[String,Any]]
    map("task") shouldBe  ("CreateClusterTask" + id+id)


    fieldValueTest(map, createExpectedWithInit)

  }

  test("create cluster service (concourse) initial script and version") {

    val map = generateCreateClusterCodeWithInitScriptAndVersion(BaseConcourseDelegateTask).asInstanceOf[Map[String,Any]]
    map("task") shouldBe  ("CreateClusterTask" + id+id)


    fieldValueTest(map, createExpectedWithInitWithVersion)

  }

  test("delete cluster service task (activiti)") {

    val xmlContent = generateDeleteClusterCode(BaseActivitiServiceTask).asInstanceOf[NodeSeq]

    (xmlContent \\ "serviceTask" \ "@name").text shouldBe "DeleteClusterTask" + id
    (xmlContent \\ "serviceTask" \ "@id").text shouldBe "serviceTask" + id.toString


    fieldValueTest(xmlContent,deleteExpected)


  }
  test("delete cluster service task (concourse)") {

    val map = generateDeleteClusterCode(BaseConcourseDelegateTask).asInstanceOf[Map[String,Any]]

    map("task") shouldBe  ("DeleteClusterTask" + id+id)



    fieldValueTest(map, deleteExpected)


  }
}
