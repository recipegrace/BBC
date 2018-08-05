package com.recipegrace.bbc.validations

import com.recipegrace.bbc.grmr.BBCStructures._
import com.recipegrace.bbc.grmr.Expressions.{NumberExpression, StringExpression}
import com.recipegrace.bbc.grmr.GrammarKeywords

/**
  * Created by Ferosh Jacob on 11/24/16.
  */
trait ClusterValidation extends BaseValidations with GrammarKeywords {

  def validateCluster(cluster: Cluster) = {

    validateClusterConfigs(cluster.clusterConfigs, cluster.variableName)
  }

  def validateClusterConfigs(clusterConfigs: List[ClusterConfig], name: String) = {
    mandatory(List(IMAGE, NUMWORKERS), clusterConfigs, s"Cluster $name")
    configValidation(clusterConfigs, s"on Cluster $name")
    clusterConfigs.foreach {
      case ClusterConfigImage(StringExpression(x)) => notEmpty(x, s"$name.image")
      case ClusterConfigWorkers(NumberExpression(x)) => {
        assert(x > 1, s"Numbers of workers should be more than one for cluster $name")
      }
      case _ =>
    }
  }
}
