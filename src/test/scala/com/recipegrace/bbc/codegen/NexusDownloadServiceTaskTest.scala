package com.recipegrace.bbc.codegen

/**
  * Created by Ferosh Jacob on 11/8/16.
  */

import com.recipegrace.bbc.activiti.BaseActivitiServiceTask
import com.recipegrace.bbc.concourse.BaseConcourseDelegateTask
import com.recipegrace.bbc.grmr.BBCStructures._
import com.recipegrace.bbc.concourse.BaseConcourseDelegateTask

import scala.xml.NodeSeq

class NexusDownloadServiceTaskTest extends BaseCodeGenTest  with NexusDownloadServiceTask {

  val id = System.currentTimeMillis().toInt
  val snapshots = "snapshots"
  val organization = "organization"
  val version = "2.0.0"
  val artifactId = "bigbricks-core"
  val cloudLocation = "gs://d/sdd"
  val expected = Map(
    "-nexusOrg" -> organization,
    "-nexusUser" -> USERNAME,
    "-nexusPassword" -> PASSWORD,
    "-nexusRepo" -> snapshots,
    "-nexusURL" -> URL,
    "-nexusVersion" -> version,
    "-nexusArtifact" -> artifactId,
    "-cloudLocation" -> cloudLocation)



  def createNexusDownload(baseTask: BaseTask)= {

    val cluster = NexusRepository(id, NEXUS, List(NexusConfigPassword(expr(PASSWORD)), NexusConfigURL(expr(URL)), NexusConfigRepositoryName(expr(snapshots)),
      NexusConfigUsername(expr(USERNAME))))
     downloadNexusJarServiceTask(cluster,
      expr( organization), expr(artifactId), expr(version), Some(expr(cloudLocation)),baseTask)
  }

  test("download nexus jar service task (activiti)") {

   val xmlContent= createNexusDownload(BaseActivitiServiceTask) .asInstanceOf[NodeSeq]

    (xmlContent \\ "serviceTask" \ "@name").text shouldBe "DownloadNexusJar" + id
    (xmlContent \\ "serviceTask" \ "@id").text shouldBe "serviceTask" + id.toString



    fieldValueTest(xmlContent, expected)


  }

  test("download nexus jar service task (concourse)") {

    val map= createNexusDownload(BaseConcourseDelegateTask) .asInstanceOf[Map[String,Any]]
    map("task") shouldBe "DownloadNexusJar" + id +id
    fieldValueTest(map, expected)


  }

}
