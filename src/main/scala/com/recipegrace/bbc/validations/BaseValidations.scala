package com.recipegrace.bbc.validations

import com.recipegrace.bbc.grmr.BBC
import com.recipegrace.bbc.grmr.BBCStructures.{RepositoryJobConfig, RepositoryJobConfigContainer, RepositoryJobConfigRepoBranch, RepositoryJobConfigRepository}
import com.recipegrace.bbc.grmr.Expressions._



/**
  * Created by Ferosh Jacob on 11/24/16.
  */
trait BaseValidations {


  val repositoryMandatoryFields = List(REPOSITORY,REPOBRANCH,CONTAINER)
  def validateRepositoryJobConfigs(repoJobConfigs: List[RepositoryJobConfig], name: String) = {

    repoJobConfigs.foreach {
      case RepositoryJobConfigRepository(StringExpression(x)) => notEmpty(x, s"$name.$REPOSITORY")
      case RepositoryJobConfigRepoBranch(StringExpression(x)) => notEmpty(x, s"$name.$REPOBRANCH")
      case RepositoryJobConfigContainer(StringExpression(x)) => notEmpty(x, s"$name.$CONTAINER")
      case _ =>
    }
  }
  def validateDistinctName[P](x: List[P], y: (P) => String, entity: String) = {
    val repeatedNames = x.map(y).groupBy(f => f).filter(f => f._2.size > 1).keys
    assert(repeatedNames.isEmpty, s"$entity has repeated declarations with the same name: ${repeatedNames.mkString(",")}")
    x.foreach(f => validateName(f, y))
  }

  def validateName[P](entity: P, y: (P) => String) = {
    assert(y(entity).nonEmpty, s"$entity name cannot be empty")
  }

  def notEmpty(value: String, name: String):Unit = {
    assert(value.nonEmpty, s"$name cannot be empty")
  }
  def notEmpty(value: Expression, name: String):Unit = {

    value.value!=None
  }

  def isValidURL(url: Expression): Boolean = {
    try {
      new java.net.URL(url.value.toString)
    } catch {
      case _: Exception => return false
    }
    true
  }

  def configValidation[T](list: List[T], message: String = "") = {
    val repeated = list.map(f => f.getClass.getName).groupBy(f => f).filter(f => f._2.size > 1).keys.map(classNameToProperty)
      assert(repeated.isEmpty, s"${repeated.mkString(",")} have repeated declaration${if (message != "") " " + message else ""}")


  }

  def mandatory[T](fields: List[String], list: List[T], message: String = "BBC") = {
    val currentFields = list.map(f => f.getClass.getName).map(classNameToProperty)
    val difference = fields.diff(currentFields)
    assert(difference.isEmpty, s"$message have missing field(s):${difference.mkString(",")}")
  }

  def classNameToProperty(fullName: String) = {
    fullName.split("\\.").last.split("\\$").last match {
      case "ProgramConfigZone" => "zone"
      case "ProgramConfigName" => "name"
      case "ProgramConfigEnv" => "env"
      case "ProgramConfigCloudWorkspace" => "cloudWorkspace"
      case "ClusterConfigWorkers" => "workers"
      case "ClusterConfigImage" => "image"
      case "ClusterConfigProperties" => "properties"
      case "ClusterConfigInitialScript" => "initialScript"
      case "ClusterConfigInitialScriptTimeOut" =>"initialScriptTimeOut"
      case "SparkJobConfigClassName" => "mainClass"
      case "SparkJobConfigProps" => "props"
      case "SparkJobConfigJarURI" => "jarLocation"
      case "PySparkJobConfigMainPyFile" => "mainPyFile"
      case "PySparkJobConfigProps" => "props"
      case "PySparkJobConfigOtherPyFiles" => "otherPyFiles"
      case "RepositoryJarURI" => "jarLocation"
      case "NexusConfigRepositoryName" => "repository"
      case "NexusConfigPassword" => "password"
      case "NexusConfigURL" => "url"
      case "NexusConfigUsername" => "username"
      case "ArtifactoryConfigRepositoryName" => "repository"
      case "ArtifactoryConfigURL" => "url"
      case "PyJobConfigMainPyFile" =>"mainPyFile"
      case "RepositoryJobConfigRepository" =>"repository"
      case "RepositoryJobConfigRepoBranch" =>"repoBranch"
      case "RepositoryJobConfigContainer" =>"container"
      case "SBTJobConfigMainClass" =>"mainClass"
      case "ArgumentSparkJobConfig"   => "args"
      case "ArgumentJavaJobConfig"   => "args"
      case "MainClassJavaJobConfig"   => "mainClass"
      case "JarLocationJavaJobConfig"   => "jarLocation"
      case "PropertiesJavaJobConfig"   => "properties"

      case x:String if x.endsWith("ConfigArgs") => "args"
      case _ => fullName

    }

  }


}