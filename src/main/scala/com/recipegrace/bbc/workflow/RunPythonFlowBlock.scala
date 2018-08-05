package com.recipegrace.bbc.workflow

import com.recipegrace.bbc.activiti.BaseActivitiServiceTask
import com.recipegrace.bbc.codegen.NexusDownloadServiceTask
import com.recipegrace.bbc.grmr.BBCStructures.{BaseRepositoryDownloadFlowBlock, ClusterStore, NexusRepository, PyJob}
import com.recipegrace.bbc.grmr.Expressions.Expression
import com.recipegrace.bbc.grmr.IDGenerator._
import com.recipegrace.bbc.concourse.{BaseConcourseSBTTask, BaseConcoursePythonTask}

import scala.xml.NodeSeq

/**
  * Created by Ferosh Jacob on 11/24/16.
  */
class RunPythonFlowBlock(pyJob: PyJob, variables:Map[String,Expression], configuration: ProgramConfiguration,clusterStore: ClusterStore)
  extends FlowBlock(configuration) with BaseConcoursePythonTask {


  override  var displayName:String = "pythonjob-"
  override var toXML: NodeSeq = {

  <notimplemented/>
  }
  override var toYAML: List[Map[String, Any]] = {
    createBasePythonConcourseTask(autoId,displayName,pyJob,variables,clusterStore,configuration)
  }
  override val flowBlockId: Int = autoId
}
