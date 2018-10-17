package com.recipegrace.bbc.workflow

import com.recipegrace.bbc.activiti.BaseActivitiServiceTask
import com.recipegrace.bbc.codegen.{ExpressionCreator, NexusDownloadServiceTask}
import com.recipegrace.bbc.composer.Templates
import com.recipegrace.bbc.grmr.BBCStructures.{BaseRepositoryDownloadFlowBlock, ClusterStore, NexusRepository, PyJob}
import com.recipegrace.bbc.grmr.Expressions.Expression
import com.recipegrace.bbc.grmr.IDGenerator._
import com.recipegrace.bbc.concourse.{BaseConcoursePythonTask, BaseConcourseSBTTask}
import com.recipegrace.bbc.grmr.IDGenerator

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

  override def template:List[KeyAndContent] = {

    object evalObject extends ExpressionCreator
    val id = IDGenerator.autoId
    val defaultArgs = Map ( "programConfiguration"->configuration, "localVariables" -> variables,"evalObject" -> evalObject)
    /* val copyJarKey = displayName +"_C"+id
     val copyJarContent = Templates.translate("templates/download-jar.ssp",Map("name" -> copyJarKey,
       "javaJob" -> javaJob) ++defaultArgs)
 */
    val runJarKey = displayName+"_R"+id
    val runJarContent = Templates.translate("templates/run-python.ssp",Map("name" -> runJarKey,
      "pythonJob" -> pyJob) ++defaultArgs)

    List(
      //KeyAndContent(copyJarKey,copyJarContent,true),
      KeyAndContent (runJarKey,runJarContent,true))
  }
}
