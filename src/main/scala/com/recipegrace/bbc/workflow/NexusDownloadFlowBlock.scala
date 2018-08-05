package com.recipegrace.bbc.workflow

import com.recipegrace.bbc.activiti.BaseActivitiServiceTask
import com.recipegrace.bbc.codegen.NexusDownloadServiceTask
import com.recipegrace.bbc.concourse.BaseConcourseDelegateTask
import com.recipegrace.bbc.grmr.BBCStructures.{BaseRepositoryDownloadFlowBlock, NexusRepository}
import com.recipegrace.bbc.grmr.Expressions.Expression
import com.recipegrace.bbc.grmr.IDGenerator._
import com.recipegrace.bbc.concourse.BaseConcourseDelegateTask

import scala.xml.NodeSeq

/**
  * Created by Ferosh Jacob on 11/24/16.
  */
class NexusDownloadFlowBlock(configuration: ProgramConfiguration, nexus: NexusRepository, organization: Expression, artifactId: Expression, version: Expression)
  extends FlowBlock(configuration) with NexusDownloadServiceTask with BaseRepositoryDownloadFlowBlock {

  override val identityString =  List(organization,artifactId,version,nexus.name).mkString("-")

  override  var displayName:String = "nexus-" +identityString
  override var toXML: NodeSeq = {

    _startID = createUserTaskId(nexus.id)
    _endID = createServiceTaskId(nexus.id)
    downloadNexusJarServiceTask(nexus, organization, artifactId, version, configuration.cloudWorkspace,BaseActivitiServiceTask).asInstanceOf[NodeSeq]
  }
  override var toYAML: List[Map[String, Any]] = {

    List( downloadNexusJarServiceTask(nexus, organization, artifactId, version, configuration.cloudWorkspace,BaseConcourseDelegateTask).asInstanceOf[Map[String,Any]])
  }
  override val flowBlockId: Int = autoId
}
