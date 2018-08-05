package com.recipegrace.bbc.codegen

/**
  * Created by Ferosh Jacob on 11/7/16.
  */

import com.recipegrace.bbc.grmr.BBCStructures._
import com.recipegrace.bbc.grmr.Expressions._

trait ArtifactoryDownloadServiceTask extends ExpressionCreator{


  def downloadArtifactoryJarServiceTask(artifactory: ArtifactoryRepository, organization: Expression, artifactId: Expression,
                                  version: Expression, cloudLocation: Option[Expression], baseTask: BaseTask) = {


    baseTask.createBaseServiceTask(artifactory.id, "DownloadArtifactoryJar" + artifactory.id,
      "com.recipegrace.bbd.main.ArtifactoryEntryPoint",
      toDownladArtifactoryJarPropertiesMap(artifactory, organization, artifactId, version, cloudLocation))
  }

  def toDownladArtifactoryJarPropertiesMap(artifactory: ArtifactoryRepository, organization: Expression, artifactId: Expression, version: Expression, cloudLocation: Option[Expression]) = {


    val cloudLocationMap =  if(cloudLocation.nonEmpty) List( "-cloudLocation" -> cloudLocation.get) else List()
    cloudLocationMap++
    List(
      "-artifactoryOrg" -> organization,
      "-artifactoryArtifact" -> artifactId,
      "-artifactoryVersion" -> version,
      "-artifactoryClassifier" -> expr("assembly")
    ) ++ fromArtifactoryConfigs(artifactory.artifactoryConfig)
  }

  def fromArtifactoryConfigs(artifactoryConfigs: List[ArtifactoryConfig]): List[(String, Expression)] = {
    artifactoryConfigs.map {
      case x: ArtifactoryConfigRepositoryName => "-artifactoryRepo" -> x.name
      case x: ArtifactoryConfigURL => "-artifactoryURL" -> x.url
    }
  }


}
