package com.recipegrace.bbc.validations

import com.recipegrace.bbc.grmr.BBCStructures._
import com.recipegrace.bbc.grmr.Expressions.StringExpression
import com.recipegrace.bbc.grmr.GrammarKeywords
import com.recipegrace.bbc.workflow.ProgramConfiguration


/**
  * Created by Ferosh Jacob on 12/24/16.
  */
trait RepositoryValidation extends BaseValidations with GrammarKeywords {

  def validateRepository(repository: Repository, programConfiguration: ProgramConfiguration) = {
    assert(programConfiguration.cloudWorkspace.nonEmpty, "cloudWorkspace cannot be empty when you use nexus")
    assert(programConfiguration.cloudWorkspace.get.value.toString.nonEmpty, "cloudWorkspace cannot be empty when you use nexus")
    repository match {
      case nexusRepository:NexusRepository =>validateNexusConfig(nexusRepository.nexusConfig, nexusRepository.name)
      case artifactoryRepository:ArtifactoryRepository =>  validateArtifactoryConfig(artifactoryRepository.artifactoryConfig, artifactoryRepository.name)
    }

  }



  def validateNexusConfig(nexusConfigs: List[NexusConfig], name: String) = {
    mandatory(List(URL, REPOSITORY), nexusConfigs, s"Nexus $name")
    configValidation(nexusConfigs, s"on Nexus Repo $name")
    nexusConfigs.foreach {
      case NexusConfigRepositoryName(StringExpression(x)) => notEmpty(x, s"$name.repository")
      case x: NexusConfigURL => assert(isValidURL(x.url), s"$name.url is not a valid URL")
      case _ =>
    }

  }
  def validateArtifactoryConfig(artifactoryConfigs: List[ArtifactoryConfig], name: String) = {
    mandatory(List(URL, REPOSITORY), artifactoryConfigs, s"Artifactory $name")
    configValidation(artifactoryConfigs, s"on Artifactory Repo $name")
    artifactoryConfigs.foreach {
      case ArtifactoryConfigRepositoryName(StringExpression(x)) => notEmpty(x, s"$name.repository")
      case x: ArtifactoryConfigURL => assert(isValidURL(x.url), s"$name.url is not a valid URL")
      case _ =>
    }

  }
}
