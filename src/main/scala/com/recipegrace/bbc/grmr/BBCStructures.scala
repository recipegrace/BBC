package com.recipegrace.bbc.grmr

import java.lang.{Boolean, Float}

import com.recipegrace.bbc.grmr.BBCStructures.RepositoryJobWrapper
import com.recipegrace.bbc.grmr.Expressions._
import com.recipegrace.bbc.workflow.FlowBlock

/**
  * Created by Ferosh Jacob on 11/7/16.
  */
object BBCStructures {

  val BIGBRICKSDELEMITER= "BIGBRICKSDELEMITER"
  def rand = {
    System.currentTimeMillis()
  }

  sealed trait ProgramConfig

  sealed trait ClusterConfig
  sealed trait Declaration


  sealed trait SparkJobConfig
  sealed trait RepositoryJobConfig
  sealed trait PyJobConfig extends RepositoryJobConfig
  sealed trait SBTJobConfig extends RepositoryJobConfig
  sealed trait WebserviceJobConfig
  sealed trait PySparkJobConfig

  sealed trait Location

  abstract class Repository(name:String){
    def getName:String = name
  }
  trait BaseRepositoryDownloadFlowBlock extends FlowBlock{
    def identityString:String
  }

  sealed trait NexusConfig
  sealed trait ArtifactoryConfig

  abstract class ActionTypeWithId(key: Int) {
    def id = key
  }

  case class ProgramConfigZone(zone: Expression) extends ProgramConfig

  case class ProgramConfigName(name: String) extends ProgramConfig

  case class ProgramConfigEnv(env: Expression) extends ProgramConfig

  case class ProgramConfigCloudWorkspace(cloudWorkspace: Expression) extends ProgramConfig

  case class ClusterConfigWorkers(workers: Expression) extends ClusterConfig
  case class ClusterConfigImage(image: Expression) extends ClusterConfig
  case class ClusterConfigProperties(props: List[Expression]) extends ClusterConfig
  case class ClusterConfigInitialScript(script: Expression) extends ClusterConfig
  case class ClusterConfigVersion(version: Expression) extends ClusterConfig
  case class ClusterConfigInitialScriptTimeOut(timeOut: Expression) extends ClusterConfig

  case class Cluster(id: Int, variableName: String, clusterConfigs: List[ClusterConfig]) extends Declaration {
    val name = "BB-"+variableName + rand

  }

  case class SparkJobConfigClassName(className: Expression) extends SparkJobConfig

  case class SparkJobConfigProps(props: Expression) extends SparkJobConfig

  case class SparkJobConfigJarURI(uri: Expression) extends SparkJobConfig

  case class RepositoryJarURI(org: Expression, artifactId: Expression, version: Expression, nexusKey: String) extends SparkJobConfig

  case class SparkJobConfigArgs(args: Array[Expression]) extends SparkJobConfig

  case class PySparkJobConfigMainPyFile(mainPyFile: Expression) extends PySparkJobConfig

  case class PySparkJobConfigProps(props: Expression) extends PySparkJobConfig

  case class PySparkJobConfigOtherPyFiles(otherPyFiles: Array[Expression]) extends PySparkJobConfig

  case class PySparkJobConfigArgs(args: Array[Expression]) extends PySparkJobConfig


  case class PyJobConfigMainPyFile(mainPyFile: Expression) extends PyJobConfig
  case class RepositoryJobConfigRepository(repository: Expression) extends PyJobConfig
  case class RepositoryJobConfigContainer(container: Expression) extends PyJobConfig
  case class RepositoryJobConfigRepoBranch(branch: Expression) extends PyJobConfig
  case class RepositoryJobConfigArgs(args: Array[Expression]) extends PyJobConfig

  case class SBTJobConfigMainClass(mainClass: Expression) extends SBTJobConfig


  case class WebserviceJobURLConfig(url:Expression) extends WebserviceJobConfig
  case class WebservicePostJobDataConfig(json:JSONType) extends WebserviceJobConfig
  case class WebserviceGetJobConfigArgs(args: Array[Expression]) extends WebserviceJobConfig


  case class GitRepository(repository:String, branch:String,name:String)

  abstract class ClusterJob(name:String,variables:List[String]) extends Declaration {
    def getName:String = name
    def getVariables:List[String]=variables

  }
  case class SparkJob(name: String, sparkJobConfigs: List[SparkJobConfig], variables:List[String]) extends ClusterJob(name,variables)
  case class PySparkJob(name: String, pySparkJobConfigs: List[PySparkJobConfig], variables:List[String]) extends ClusterJob(name,variables)

  class RepositoryJobWrapper(repository: Expression,container:Expression, branch:Expression,name:String) {
    def getRepository = repository
    def getBranch = branch
    def getName = name
    def getContainer =container
  }
  case class PyJobWrapper (mainPyFile:Expression,  repository:Expression, container:Expression,
                            branch:Expression, name:String) extends RepositoryJobWrapper(repository,container,branch,name)
  case class SBTJobWrapper(mainClass:Expression,  repository:Expression, branch:Expression,
                            container:Expression,name:String) extends RepositoryJobWrapper(repository,container,branch,name)

  case class WebservicePostJobWrapper(url:Expression, json:JSONType)
  case class WebserviceGetJobWrapper(url:Expression)


  abstract class RepositoryJob(name:String,variables:List[String],repositoryJobConfigs:List[RepositoryJobConfig]) extends Declaration {
    def getName:String = name
    def getVariables:List[String]=variables
    lazy val repositoryJob = calculateWrapper()
    lazy val configMap = repositoryJobConfigs
      .map {
        case x: RepositoryJobConfigRepository => REPOSITORY -> x.repository
        case x: RepositoryJobConfigContainer => CONTAINER -> x.container
        case x: RepositoryJobConfigRepoBranch => REPOBRANCH -> x.branch
        case _ => (System.currentTimeMillis()+"") -> StringExpression(System.currentTimeMillis()+"" )

      }.toMap
    def calculateWrapper() = {

      new RepositoryJobWrapper(configMap(REPOSITORY), configMap(CONTAINER), configMap(REPOBRANCH),name)
    }
  }


  case class PipelineJob(name:String, actions:List[ActionTypeWithId],variables:List[String]) extends Declaration
  case class PyJob(name: String, pyJobConfigs: List[RepositoryJobConfig], variables:List[String]) extends RepositoryJob(name,variables,pyJobConfigs) {
    lazy val pyJob = calculateWrapper()

    override def calculateWrapper() = {
      val pyConfigs = pyJobConfigs
        .map {
        case x: PyJobConfigMainPyFile => MAINPYFILE -> x.mainPyFile
        case _ => (System.currentTimeMillis()+"") -> StringExpression(System.currentTimeMillis()+"" )

      }.toMap ++configMap
      PyJobWrapper(pyConfigs(MAINPYFILE), pyConfigs(REPOSITORY), pyConfigs(CONTAINER), pyConfigs(REPOBRANCH),name)
    }
  }


  case class SBTJob(name: String, sbtJobConfigs: List[RepositoryJobConfig], variables:List[String]) extends RepositoryJob(name,variables,sbtJobConfigs) {
    lazy val sbtJob = calculateWrapper()

    override def calculateWrapper() = {
      val sbtConfigs = sbtJobConfigs
        .map {
          case x: SBTJobConfigMainClass => MAINCLASS -> x.mainClass
          case _ => (System.currentTimeMillis()+"") -> StringExpression(System.currentTimeMillis()+"" )

        }.toMap ++ configMap
      SBTJobWrapper(sbtConfigs(MAINCLASS), sbtConfigs(REPOSITORY), sbtConfigs(REPOBRANCH),sbtConfigs(CONTAINER),name)
    }
  }

  case class WebservicePostJob(name: String, webserviceJobConfigs: List[WebserviceJobConfig], variables:List[String]) extends Declaration {
    lazy val sbtJob = calculateWrapper()

    def calculateWrapper() = {
      val webserviceConfigs = webserviceJobConfigs
        .map {
          case x: WebserviceJobURLConfig => URL -> x.url
          case x: WebservicePostJobDataConfig => JSON -> x.json
          case _ => (System.currentTimeMillis()+"") -> StringExpression(System.currentTimeMillis()+"" )

        }.toMap
      WebservicePostJobWrapper(webserviceConfigs(URL), webserviceConfigs(JSON).asInstanceOf[JSONType])
    }
  }
  case class WebserviceGetJob(name: String, webserviceJobConfigs: List[WebserviceJobConfig], variables:List[String]) extends Declaration {
    lazy val sbtJob = calculateWrapper()

    def calculateWrapper() = {
      val webserviceConfigs = webserviceJobConfigs
        .map {
          case x: WebserviceJobURLConfig => URL -> x.url
          case _ => (System.currentTimeMillis()+"") -> StringExpression(System.currentTimeMillis()+"" )

        }.toMap
      WebserviceGetJobWrapper(webserviceConfigs(URL))
    }
  }
  case class GCloudLocation(bucketLocation: String)

  case class RepositoryLocation(groupId: Expression, artifactId: Expression, version: Expression, repository: Repository)

  case class NexusConfigRepositoryName(name: Expression) extends NexusConfig

  case class NexusConfigPassword(password: Expression) extends NexusConfig

  case class NexusConfigURL(url: Expression) extends NexusConfig

  case class NexusConfigUsername(username: Expression) extends NexusConfig


  case class ArtifactoryConfigRepositoryName(name: Expression) extends ArtifactoryConfig


  case class ArtifactoryConfigURL(url: Expression) extends ArtifactoryConfig



  case class NexusRepository(id: Int, name: String, nexusConfig: List[NexusConfig]) extends Repository(name) with Declaration
  case class ArtifactoryRepository(id: Int, name: String, artifactoryConfig: List[ArtifactoryConfig])extends Repository(name) with Declaration

  sealed trait VariableValueType
  object BooleanType extends VariableValueType
  object StringType extends VariableValueType
  object IntType extends  VariableValueType
  object FloatType extends VariableValueType
  object EmptyVariableType extends VariableValueType




  var variableStore:Map[String, Expression] = Map()
 case class VariableDeclaration(name:String, valueOption:Option[Expression]) extends Declaration {

    def value = valueOption.get

    def isProcessVariable:Boolean = valueOption.isEmpty

  }
  case class VariableDeclarations (variables:List[VariableDeclaration]) extends Declaration



  case class JSONDeclaration(name: String, content: JSONType with Product with Serializable) extends Declaration
  case class DeleteGCSFolderAction(folder: Expression, identifier: Int) extends ActionTypeWithId(identifier)

  case class RunJobAction(job: String, cluster: Option[String], identifier: Int, variables:List[Expression]) extends ActionTypeWithId(identifier)


  case class ClusterStore(running: List[String])

  case object EmptyProgramConfig extends ProgramConfig

}
