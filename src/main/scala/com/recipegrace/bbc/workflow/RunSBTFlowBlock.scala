package com.recipegrace.bbc.workflow

import com.recipegrace.bbc.concourse.BaseConcourseSBTTask
import com.recipegrace.bbc.grmr.BBCStructures.{ClusterStore, SBTJob, PyJob}
import com.recipegrace.bbc.grmr.Expressions.Expression
import com.recipegrace.bbc.grmr.IDGenerator._

import scala.xml.NodeSeq

/**
  * Created by Ferosh Jacob on 11/24/16.
  */
class RunSBTFlowBlock(pyJob: SBTJob, variables:Map[String,Expression], configuration: ProgramConfiguration, clusterStore: ClusterStore)
  extends FlowBlock(configuration) with BaseConcourseSBTTask {


  override  var displayName:String = "sbtjob-"
  override var toXML: NodeSeq = {

  <notimplemented/>
  }
  override var toYAML: List[Map[String, Any]] = {
    createBaseSBTConcourseTask(autoId,displayName,pyJob,variables,clusterStore,configuration)
  }
  override val flowBlockId: Int = autoId
}
