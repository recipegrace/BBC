package com.recipegrace.bbc.codegen

/**
  * Created by Ferosh Jacob on 11/8/16.
  */

import com.recipegrace.bbc.activiti.BaseActivitiServiceTask
import com.recipegrace.bbc.concourse.BaseConcourseDelegateTask
import com.recipegrace.bbc.grmr.BBCStructures._
import com.recipegrace.bbc.concourse.BaseConcourseDelegateTask

import scala.xml.NodeSeq

class ArtifactoryDownloadServiceTaskTest extends BaseCodeGenTest with ArtifactoryDownloadServiceTask {

  val id = System.currentTimeMillis().toInt
  val snapshots = "snapshots"
  val organization = "organization"
  val version = "2.0.0"
  val artifactId = "bigbricks-core"
  val cloudLocation = "gs://d/sdd"
  val expected = Map(
    "-artifactoryOrg" -> organization,
    "-artifactoryRepo" -> snapshots,
    "-artifactoryURL" -> URL,
    "-artifactoryVersion" -> version,
    "-artifactoryArtifact" -> artifactId,
    "-cloudLocation" -> cloudLocation)



  def createArtifactoryDownload(baseTask: BaseTask)= {

    val cluster = ArtifactoryRepository(id, ARTIFACTORY, List(ArtifactoryConfigURL(expr(URL)),
      ArtifactoryConfigRepositoryName(expr(snapshots))
      ))
     downloadArtifactoryJarServiceTask(cluster,
      expr( organization), expr(artifactId), expr(version), Some(expr(cloudLocation)),baseTask)
  }

  test("download artifactory jar service task (activiti)") {

   val xmlContent= createArtifactoryDownload(BaseActivitiServiceTask) .asInstanceOf[NodeSeq]

    (xmlContent \\ "serviceTask" \ "@name").text shouldBe "DownloadArtifactoryJar" + id
    (xmlContent \\ "serviceTask" \ "@id").text shouldBe "serviceTask" + id.toString



    fieldValueTest(xmlContent, expected)


  }

  test("download artifactory jar service task (concourse)") {

    val map= createArtifactoryDownload(BaseConcourseDelegateTask) .asInstanceOf[Map[String,Any]]
    map("task") shouldBe "DownloadArtifactoryJar" + id +id
    fieldValueTest(map, expected)


  }

}
