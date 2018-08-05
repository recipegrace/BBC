package com.recipegrace.bbc.workflow

import com.recipegrace.bbc.grmr.BBCStructures._


trait ClusterStoreIO {

  def isClusterRunning(clusterStore: ClusterStore, clusterName: String): Boolean = {
    clusterStore.running.contains(clusterName)
  }

  def addToClusterStore(clusterName: String, clusterStore: ClusterStore): ClusterStore = {
    assert(!clusterStore.running.contains(clusterName), s"Clusterstore already has cluster $clusterName")
    ClusterStore(clusterName :: clusterStore.running)
  }

  def isClusterNeededLater(nextActions: List[ActionTypeWithId], cluster: String, allDeclarationsMap: Map[String, Declaration]):Boolean = {
  val result=  nextActions.exists {
      case action: RunJobAction => allDeclarationsMap(action.job) match {
        case x:ClusterJob if  action.cluster.get == cluster => true
        case x:PipelineJob => isClusterNeededLater(x.actions,cluster,allDeclarationsMap)
        case _=>false
      }
      case _ => false
    }
    result
  }

  def removeFromClusterStore(clusterName: String, clusterStore: ClusterStore): ClusterStore = {
    assert(clusterStore.running.contains(clusterName), s"Clusterstore doesn't have cluster $clusterName")
    ClusterStore(clusterStore.running.filter(f => f != clusterName))
  }
}