package com.recipegrace.bbc.codegen

/**
  * Created by Ferosh Jacob on 11/7/16.
  */

import com.recipegrace.bbc.grmr.BBCStructures._
import com.recipegrace.bbc.grmr.Expressions._
trait ClusterServiceTask extends ExpressionCreator{


  def createClusterServiceTask(cluster: Cluster, env: Expression, zone: Expression, baseTask: BaseTask) = {


   baseTask.createBaseServiceTask(cluster.id, "CreateClusterTask" + cluster.id,
      "com.recipegrace.bbd.main.CreateClusterEntryPoint",
      toCreateClusterPropertiesMap(cluster, env, zone))
  }


  def toCreateClusterPropertiesMap(cluster: Cluster, env: Expression, zone: Expression):List[(String,Expression)] = {
    List(
      "-region" -> expr("global"),
      "-tags" -> expr("dataproc"),
      "-network" -> expr("internal"),
      "-clusterName" -> expr(cluster.name),
      "-zone" -> zone,
      "-environment" -> env) ++ fromClusterConfigs(cluster.clusterConfigs)
  }

  def fromClusterConfigs(clusterConfigs: List[ClusterConfig]): List[(String, Expression)] = {
    clusterConfigs.flatMap{
      case x: ClusterConfigImage => List("-image" -> x.image)
      case x: ClusterConfigProperties => x.props.map(g=>   "-properties" -> g)
      case x: ClusterConfigWorkers => List("-numOfWorkers" -> x.workers)
      case x: ClusterConfigInitialScript => List("-initialScript" -> x.script)
      case x: ClusterConfigVersion => List("-version" -> x.version)
      case x: ClusterConfigInitialScriptTimeOut => List("-initialScriptTimeOut" -> x.timeOut)
      case x: ClusterConfigSubNetwork => List("-subNetwork" -> x.subNetwork)
        //Temp fix!!
      case x: ClusterConfigTags => List("-tags" -> expr( x.tags.mkString(",")))

    }
  }

  def deleteClusterServiceTask(clusterName: List[String], env: Expression, id: Int, baseTask: BaseTask) = {
   baseTask.createBaseServiceTask(id, "DeleteClusterTask" + id,
      "com.recipegrace.bbd.main.DeleteClusterEntryPoint",
      toDeleteClusterPropertiesMap(clusterName, env, id))
  }

  def toDeleteClusterPropertiesMap(clusterName: List[String], env: Expression, id: Int): List[(String, Expression)] = {
    List(
      "-clusterName" ->expr( clusterName.distinct.mkString(",")),
      "-region" -> expr("global"),
      "-environment" -> env)
  }


}
