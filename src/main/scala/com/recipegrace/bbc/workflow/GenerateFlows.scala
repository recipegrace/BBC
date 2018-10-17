package com.recipegrace.bbc.workflow

import com.recipegrace.bbc.activiti.ActivitiFlowBlockCombiner
import com.recipegrace.bbc.codegen.ExpressionCreator
import com.recipegrace.bbc.grmr.BBCStructures._
import com.recipegrace.bbc.grmr.Expressions.Expression
import com.recipegrace.bbc.grmr._
import java.util.logging.Logger

import com.recipegrace.bbc.composer.ComposerFlowBlockCombiner
import com.recipegrace.bbc.concourse.ConcourseFlowBlockCombiner
import com.recipegrace.bbc.validations.BBCValidations
/**
  * Created by Ferosh Jacob on 11/24/16.
  */
class GenerateFlows(bBC: BBC) extends ClusterStoreIO with  GrammarKeywords with ExpressionCreator {

  val logger = Logger.getLogger(this.getClass.getName)

  private val clusterMap: Map[String, Cluster] = bBC.clusters.map(f => f.variableName -> f).toMap
  private val repositoryMap:Map[String,Repository] = bBC.repositories.map(f=> f.getName->f).toMap
  var clusterStore = new ClusterStore(List())




  def generateFlows: List[FlowBlock] = {
    val flowblocks=toFlowBlock(bBC.actions,Map())
    logger.info("stages:" + flowblocks.mkString(","))
    flowblocks
  }


  def generateActiviti= {

    ActivitiFlowBlockCombiner.wrapWorkflow(repositoryFlowBlocks(bBC.actions) ++ toFlowBlock(bBC.actions,Map()), bBC.programConfiguration.name)
  }

  def generateConcourse = {
    ConcourseFlowBlockCombiner.mergeYAML(repositoryFlowBlocks(bBC.actions)++ toFlowBlock(bBC.actions,Map()),findResources, bBC.programConfiguration.name)
  }
  def generateComposer = {
    ComposerFlowBlockCombiner.mergeWorkflow(repositoryFlowBlocks(bBC.actions)++ toFlowBlock(bBC.actions,Map()),findResources, bBC.programConfiguration)
  }


  def findResources = {

    if(    bBC.activeRepositoryJobs(bBC.actions).groupBy(f=>f.getName).map(f=>f._2.head).toList.exists(f=> f.repositoryJob.getBranch.isEmpty)) List()
    else bBC.activeRepositoryJobs(bBC.actions).groupBy(f=>f.getName).map(f=>f._2.head).toList.map(f=> GitRepository( f.repositoryJob.getRepository.get.value+"", f.repositoryJob.getBranch.get.value+"",f.repositoryJob.getName))

  }
  def createDownloadBlock(programConfiguration: ProgramConfiguration, repository: Repository, org: Expressions.Expression,
                          artifactId: Expressions.Expression, version: Expressions.Expression) = {

    repository match {
      case nexus:NexusRepository=> new NexusDownloadFlowBlock(programConfiguration,nexus,org,artifactId,version)
      case artifactory:ArtifactoryRepository => new ArtifactoryDownloadFlowBlock(programConfiguration,artifactory,org,artifactId,version)
    }
  }

  private def repositoryFlowBlocks(actions: List[ActionTypeWithId]): List[FlowBlock] = {


    def allSparkJobConfigs(actions:List[ActionTypeWithId]):List[BaseSparkJobConfig] = actions.flatMap{
      case x:RunJobAction => List(x.job)
      case _ => List()
    }.map(f=> bBC.allDeclarationsMap(f)).flatMap{
      case x:SparkJob => x.sparkJobConfigs
      case x:PipelineJob => allSparkJobConfigs(x.actions)
      case _ => List()
    }

    val repositoryflowBlocks = allSparkJobConfigs(bBC.actions).flatMap {
      case RepositoryJarURI(org, artifactId, version, repositoryKey) => List(createDownloadBlock(bBC.programConfiguration, repositoryMap(repositoryKey), org, artifactId, version))
      case _ => List()
    }


    //createDownloadBlock(bBC.programConfiguration,repositoryMap(repositoryKey) , org, artifactId, version)



      repositoryflowBlocks.groupBy(f=>f.identityString).map(f=> f._2.head).toList
  }

  var nextActionsInPipeLine:List[ActionTypeWithId] = List()
  def processJobAction(x: RunJobAction,actions: List[ActionTypeWithId],currentLocalVariables:Map[String,Expression]): List[FlowBlock] = {


    val output=bBC.allDeclarationsMap(x.job) match {
      case f:ClusterJob => {
        val functionCall = f.getVariables.zip( x.variables.map(f=>expr(evaluateVariable(f,currentLocalVariables)+""))).toMap
        createRunSparkActionFlowBlock(x, actions++nextActionsInPipeLine,functionCall) :: toFlowBlock(actions,currentLocalVariables)
      }
      case f:PyJob =>  new RunPythonFlowBlock(f,f.variables.zip(x.variables.map(f=>expr(evaluateVariable(f,currentLocalVariables)+""))).toMap,bBC.programConfiguration,clusterStore) :: toFlowBlock(actions,currentLocalVariables)
      case f:SBTJob =>  new RunSBTFlowBlock(f,f.variables.zip(x.variables.map(f=>expr(evaluateVariable(f,currentLocalVariables)+""))).toMap,bBC.programConfiguration,clusterStore) :: toFlowBlock(actions,currentLocalVariables)
      case f:JavaJob =>  new RunJavaFlowBlock(f,f.variables.zip(x.variables.map(f=>expr(evaluateVariable(f,currentLocalVariables)+""))).toMap,bBC.programConfiguration,clusterStore) :: toFlowBlock(actions,currentLocalVariables)
      case f:PipelineJob => {
        nextActionsInPipeLine = actions.flatMap{
          case x:RunJobAction => bBC.allDeclarationsMap(x.job) match {
            case y:PipelineJob =>y.actions
            case z:ClusterJob => List(x)
            case _=> List()
          }
          case _=> List()
        }
        val flowBlocks= toFlowBlock(f.actions,f.variables.zip(x.variables).toMap)
        nextActionsInPipeLine= List()
       flowBlocks ++ toFlowBlock(actions,currentLocalVariables)
      }

    }

    output
  }

  private def toFlowBlock(actions: List[ActionTypeWithId], currentLocalVariables:Map[String,Expression]): List[FlowBlock] = {
    actions match {
      case Nil => List()
      case x :: y => x match {
        case x:RunJobAction => processJobAction(x,y,currentLocalVariables)
        case x: DeleteGCSFolderAction => new DeleteGCSActionFlowBlock(bBC.programConfiguration, x.folder,currentLocalVariables) :: toFlowBlock(y,currentLocalVariables)
      }
    }
  }


  private def createRunSparkActionFlowBlock(x: RunJobAction, actions: List[ActionTypeWithId], localVariables:Map[String, Expression]) = {


    val cluster = clusterMap(x.cluster.get)
    val clusterName = cluster.name
    val clusterCreate = !clusterStore.running.contains(clusterName)
    if (clusterCreate) clusterStore = addToClusterStore(clusterName, clusterStore)

    val clusterDelete = !isClusterNeededLater(actions, x.cluster.get, bBC.allDeclarationsMap)
    if (clusterDelete) clusterStore = removeFromClusterStore(clusterName, clusterStore)


    new RunSparkActionFlowBlock(clusterCreate, clusterDelete,
    cluster, bBC.allDeclarationsMap(x.job).asInstanceOf[ClusterJob], bBC.programConfiguration,localVariables,clusterStore
    )
  }
}

object GenerateFlows extends BBCValidations {
  def apply(bBC: BBC) = {
    validateBBC(bBC)
    new GenerateFlows(bBC)
  }

}