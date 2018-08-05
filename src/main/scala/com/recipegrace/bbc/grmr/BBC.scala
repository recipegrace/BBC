package com.recipegrace.bbc.grmr

import com.recipegrace.bbc.grmr.Expressions._
import com.recipegrace.bbc.grmr.BBCStructures._
import com.recipegrace.bbc.workflow.ProgramConfiguration
/**
  * Created by Ferosh Jacob on 12/29/16.
  */
class BBC(val programConfigs: List[ProgramConfig],
          val declarations: List[(String,Declaration)], val actions: List[ActionTypeWithId]) extends GrammarKeywords {


  lazy val storeAllDeclarations:List[(String, Declaration)] = declarations.flatMap(f=> f._2 match {
    case x:VariableDeclarations =>   x.variables.map(f=> f.name -> f)
    case _=> List(f._1 -> f._2 )
  })



  def allDeclarations=storeAllDeclarations
  def allDeclarationsMap = storeAllDeclarations.toMap



  def getDeclarations[T](implicit m: Manifest[T]) = allDeclarations.flatMap(f=> f._2 match {
    case x:T   => List(x)
    case _ => List()
  })
  def activeRepositoryJobs(fromActions:List[ActionTypeWithId]=actions ):List[RepositoryJob] = {
    fromActions.flatMap{
      case x:RunJobAction => allDeclarationsMap(x.job) match{
        case x:RepositoryJob => List(x)
        case x:PipelineJob =>   activeRepositoryJobs(x.actions)
        case _ => List()
      }
      case _=> List()
    }
  }

  val sparkJobs: List[SparkJob] = getDeclarations[SparkJob]
  val pySparkJobs: List[PySparkJob] = getDeclarations[PySparkJob]
  val pyJobs: List[PyJob] = getDeclarations[PyJob]
  val sbtJobs: List[SBTJob] = getDeclarations[SBTJob]

  val clusters: List[Cluster] = getDeclarations[Cluster]
  val repositories: List[Repository] =getDeclarations[NexusRepository]++ getDeclarations[ArtifactoryRepository]
  lazy val storeVariables :List[VariableDeclaration] = allDeclarations.flatMap{ f=> f._2 match {
    case x: VariableDeclaration => List(x)
    case _ => List()
  }
  }

  def variables = storeVariables

  def programConfiguration = {


    val configs = programConfigs
      .filter({
        case EmptyProgramConfig => false
        case _ => true
      })
      .map({
        case x: ProgramConfigEnv => ENV -> x.env
        case x: ProgramConfigName => SIMPLENAME -> StringExpression(x.name)
        case x: ProgramConfigZone => ZONE -> x.zone
        case x: ProgramConfigCloudWorkspace => CLOUDWORKSPACE -> x.cloudWorkspace

      }).toMap
    ProgramConfiguration(configs(ENV), configs(ZONE), configs(SIMPLENAME).value.toString, configs.get(CLOUDWORKSPACE))
  }

}

