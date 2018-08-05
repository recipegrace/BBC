package com.recipegrace.bbc.workflow

import com.recipegrace.bbc.activiti.BaseActivitiServiceTask
import com.recipegrace.bbc.codegen.ArtifactoryDownloadServiceTask
import com.recipegrace.bbc.concourse.BaseConcourseDelegateTask
import com.recipegrace.bbc.grmr.BBCStructures.{ArtifactoryRepository, BaseRepositoryDownloadFlowBlock}
import com.recipegrace.bbc.grmr.Expressions.Expression
import com.recipegrace.bbc.grmr.IDGenerator._
import com.recipegrace.bbc.concourse.BaseConcourseDelegateTask

import scala.xml.NodeSeq

/**
  * Created by Ferosh Jacob on 11/24/16.
  */
class ArtifactoryDownloadFlowBlock(configuration: ProgramConfiguration, artifactory: ArtifactoryRepository, organization: Expression, artifactId: Expression, version: Expression)
  extends FlowBlock(configuration) with ArtifactoryDownloadServiceTask with BaseRepositoryDownloadFlowBlock {

  override  val identityString =  List(organization,artifactId,version,artifactory.name).mkString("-")

  override  var displayName:String = "artifactory-" +identityString
  override var toXML: NodeSeq = {

    _startID = createUserTaskId(artifactory.id)
    _endID = createServiceTaskId(artifactory.id)
    downloadArtifactoryJarServiceTask(artifactory, organization, artifactId, version, configuration.cloudWorkspace,BaseActivitiServiceTask).asInstanceOf[NodeSeq]
  }
  override var toYAML: List[Map[String, Any]] = {

    List( downloadArtifactoryJarServiceTask(artifactory, organization, artifactId, version, configuration.cloudWorkspace,BaseConcourseDelegateTask).asInstanceOf[Map[String,Any]])
  }
  override val flowBlockId: Int = autoId
}
