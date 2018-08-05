package com.recipegrace.bbc.codegen

/**
  * Created by Ferosh Jacob on 11/7/16.
  */

import com.recipegrace.bbc.grmr.BBCStructures._
import com.recipegrace.bbc.grmr.Expressions._

trait NexusDownloadServiceTask extends ExpressionCreator{


  def downloadNexusJarServiceTask(nexus: NexusRepository, organization: Expression, artifactId: Expression,
                                  version: Expression, cloudLocation: Option[Expression], baseTask: BaseTask) = {


    baseTask.createBaseServiceTask(nexus.id, "DownloadNexusJar" + nexus.id,
      "com.recipegrace.bbd.main.NexusEntryPoint",
      toDownladNexusJarPropertiesMap(nexus, organization, artifactId, version, cloudLocation))
  }

  def toDownladNexusJarPropertiesMap(nexus: NexusRepository, organization: Expression, artifactId: Expression, version: Expression, cloudLocation: Option[Expression]) = {


    val cloudLocationMap =  if(cloudLocation.nonEmpty) List( "-cloudLocation" -> cloudLocation.get) else List()
    cloudLocationMap++
      List(
      "-nexusOrg" -> organization,
      "-nexusArtifact" -> artifactId,
      "-nexusVersion" -> version,
      "-nexusClassifier" -> expr("assembly")
    ) ++ fromNexusConfigs(nexus.nexusConfig)
  }

  def fromNexusConfigs(nexusConfigs: List[NexusConfig]): List[(String, Expression)] = {
    nexusConfigs.map {
      case x: NexusConfigUsername => "-nexusUser" -> x.username
      case x: NexusConfigPassword => "-nexusPassword" -> x.password
      case x: NexusConfigRepositoryName => "-nexusRepo" -> x.name
      case x: NexusConfigURL => "-nexusURL" -> x.url
    }
  }


}
