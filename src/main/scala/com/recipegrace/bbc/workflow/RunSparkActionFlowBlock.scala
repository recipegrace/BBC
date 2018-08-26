package com.recipegrace.bbc.workflow

import com.recipegrace.bbc.activiti.BaseActivitiServiceTask
import com.recipegrace.bbc.codegen.{ClusterServiceTask, ExpressionCreator, SubmitPySparkJobServiceTask, SubmitSparkJobServiceTask}
import com.recipegrace.bbc.composer.Templates
import com.recipegrace.bbc.concourse.BaseConcourseDelegateTask
import com.recipegrace.bbc.grmr.BBCStructures._
import com.recipegrace.bbc.grmr.Expressions.Expression
import com.recipegrace.bbc.grmr.IDGenerator._
import com.recipegrace.bbc.concourse.BaseConcourseDelegateTask
import com.recipegrace.bbc.grmr.IDGenerator
import com.recipegrace.bbc.workflow

import scala.xml.NodeSeq

/**
  * Created by Ferosh Jacob on 11/24/16.
  */
class RunSparkActionFlowBlock(clusterCreate: Boolean, clusterDelete: Boolean, cluster: Cluster,
                              clusterJob: ClusterJob, config: ProgramConfiguration, variables:Map[String,Expression], clusterStore: ClusterStore)
  extends FlowBlock(config: ProgramConfiguration)
    with ClusterServiceTask with SubmitSparkJobServiceTask  with SubmitPySparkJobServiceTask{

  override var displayName:String = clusterJob.getName

  override var toXML: NodeSeq = {

    val sparkJobID = autoId
    val createClusterXML = if (clusterCreate) {
      _startID = createUserTaskId(cluster.id)
      createClusterServiceTask(cluster, config.env, config.zone,BaseActivitiServiceTask).asInstanceOf[NodeSeq]
    }
    else {
      _startID = createUserTaskId(sparkJobID)
      NodeSeq.Empty
    }
    val runSparkXML = clusterJob match {
      case sparkJob: SparkJob => createSparkJobServiceTask(sparkJobID, sparkJob, config,
        cluster.name, BaseActivitiServiceTask, variables).asInstanceOf[NodeSeq] ++ createConnect(clusterCreate, cluster.id, sparkJobID)
      case pySparkJob: PySparkJob => createPySparkJobServiceTask(sparkJobID, pySparkJob, config,
        cluster.name, BaseActivitiServiceTask, variables).asInstanceOf[NodeSeq] ++ createConnect(clusterCreate, cluster.id, sparkJobID)
    }
    val deleteClusterXML = if (clusterDelete) {
      val deleteTaskId = autoId
      _endID = createServiceTaskId(deleteTaskId)
      deleteClusterServiceTask(List(cluster.name), config.env, deleteTaskId,BaseActivitiServiceTask).asInstanceOf[NodeSeq] ++ createConnect(sparkJobID, deleteTaskId)
    } else {
      _endID = createServiceTaskId(sparkJobID)
      NodeSeq.Empty
    }

    val xml = createClusterXML ++ runSparkXML ++ deleteClusterXML
    //println(xml)
    xml
  }

  def createConnect(clusterCreate: Boolean, from: Int, to: Int): NodeSeq = {

    if (clusterCreate)
      createConnect(from, to)
    else NodeSeq.Empty
  }


  override var toYAML: List[Map[String, Any]] = {
    val sparkJobID = autoId


     val createYAML = if(clusterCreate)     List( createClusterServiceTask(cluster, config.env, config.zone,BaseConcourseDelegateTask).asInstanceOf[Map[String,Any]])
     else List()
     val submitYAML=  clusterJob match {
       case sparkJob:SparkJob =>createSparkJobServiceTask(sparkJobID, sparkJob, config, cluster.name,BaseConcourseDelegateTask,variables).asInstanceOf[Map[String,Any]]
       case pySparkJob:PySparkJob =>createPySparkJobServiceTask(sparkJobID, pySparkJob, config, cluster.name,BaseConcourseDelegateTask,variables).asInstanceOf[Map[String,Any]]

     }
     val withFailure = List(submitYAML++Map("on_failure"-> deleteClusterServiceTask(cluster.name::clusterStore.running, config.env,autoId,BaseConcourseDelegateTask).asInstanceOf[Map[String,Any]]))

     val deleteYAML= if(clusterDelete)   List(deleteClusterServiceTask(List(cluster.name), config.env,autoId,BaseConcourseDelegateTask).asInstanceOf[Map[String,Any]]) else List()

    createYAML++withFailure++deleteYAML



  }
  override val flowBlockId: Int = autoId
  override def template:List[KeyAndContent] = {

    object evalObject extends ExpressionCreator
    val id = IDGenerator.autoId
    val defaultArgs = Map ( "programConfiguration"->config, "localVariables" -> variables,"evalObject" -> evalObject)
    val clusterCreateKey = displayName +"_C"+id
    val clusterCreation = if(clusterCreate) Templates.translate("templates/create-cluster.ssp",Map("name" -> clusterCreateKey,
      "cluster" -> cluster) ++defaultArgs) else s"# Reusing cluster ${cluster.name}"

    val submitSparkJobKey = displayName+"_S"+id
    val submitSparkJob = clusterJob match {
      case sparkJob: SparkJob => KeyAndContent(submitSparkJobKey,Templates.translate("templates/run-spark.ssp",Map("name" -> submitSparkJobKey,
        "clusterName" -> cluster.name, "sparkJob" -> sparkJob)++defaultArgs),true)
      case sparkJob: PySparkJob =>KeyAndContent(submitSparkJobKey ,Templates.translate("templates/run-pyspark.ssp",Map("name" -> submitSparkJobKey,
        "clusterName" -> cluster.name, "sparkJob" -> sparkJob)++defaultArgs),true)
    }

    val clusterDeleteKey = displayName+"_D"+id
    val clusterDeletion = if(clusterDelete) Templates.translate("templates/delete-cluster.ssp",Map("name" -> clusterDeleteKey,
      "clusterName" -> cluster.name)) else s"# Don't delete the  cluster ${cluster.name}, will be reused"

    List( KeyAndContent(clusterCreateKey,clusterCreation,clusterCreate), submitSparkJob, KeyAndContent(clusterDeleteKey,clusterDeletion,clusterDelete))
  }

}
