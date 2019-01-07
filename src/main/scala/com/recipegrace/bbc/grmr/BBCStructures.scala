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



  sealed trait JavaJobConfig
  sealed trait BaseSparkJobConfig
  sealed trait SparkJobConfig extends BaseSparkJobConfig
  sealed trait RepositoryJobConfig

  sealed trait PyJobConfig extends RepositoryJobConfig
  sealed trait SBTJobConfig extends RepositoryJobConfig
  sealed trait WebserviceJobConfig
  sealed trait PySparkJobConfig extends BaseSparkJobConfig
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

  case class ArgumentJavaJobConfig(args:Array[Expression]) extends JavaJobConfig
  case class MainClassJavaJobConfig(mainClass:Expression) extends JavaJobConfig
  case class JarLocationJavaJobConfig(jarLocation:Expression) extends JavaJobConfig
  case class PropertiesJavaJobConfig(properties:Expression) extends JavaJobConfig


  case class ArgumentSparkJobConfig(args:Array[Expression]) extends BaseSparkJobConfig
  case class ProgramConfigZone(zone: Expression) extends ProgramConfig

  case class ProgramConfigName(name: String) extends ProgramConfig

  case class ProgramConfigEnv(env: Expression) extends ProgramConfig

  case class ProgramConfigCloudWorkspace(cloudWorkspace: Expression) extends ProgramConfig

  case class ClusterConfigWorkers(workers: Expression) extends ClusterConfig
  case class ClusterConfigImage(image: Expression) extends ClusterConfig
  case class ClusterConfigDataProcVersion(version:Expression) extends ClusterConfig
  case class ClusterConfigSubNetwork(subNetwork: Expression) extends ClusterConfig
  case class ClusterConfigTags(tags: Array[Expression]) extends ClusterConfig
  case class ClusterConfigProperties(props: List[Expression]) extends ClusterConfig
  case class ClusterConfigInitialScript(script: Expression) extends ClusterConfig
  case class ClusterConfigVersion(version: Expression) extends ClusterConfig
  case class ClusterConfigInitialScriptTimeOut(timeOut: Expression) extends ClusterConfig


  case class ClusterWrapper(workers:Expression,image:Expression,initialScript:Option[Expression],initialActionTimeout:Option[Expression], subNetWork:Option[Expression], tags:Option[Array[Expression]], version:Option[Expression])
  case class Cluster(id: Int, variableName: String, clusterConfigs: List[ClusterConfig]) extends Declaration {
    val name = "BB-"+variableName + rand
    lazy val cluster = calculateWrapper()


    val tagMap:Map[String, Array[Expression]] = clusterConfigs
      .map {

        case x:ClusterConfigTags => TAGS ->x.tags
        case _ => (System.currentTimeMillis()+"") -> Array(StringExpression(System.currentTimeMillis()+"" ).asInstanceOf[Expression])

      }.toMap

    lazy val configMap = clusterConfigs
      .map {
        case x: ClusterConfigWorkers => NUMWORKERS -> x.workers
        case x: ClusterConfigImage => IMAGE -> x.image
        case x: ClusterConfigInitialScript =>INITIALSCRIPT -> x.script
        case x: ClusterConfigSubNetwork =>SUBNETWORK -> x.subNetwork
        case x: ClusterConfigInitialScriptTimeOut =>INITIALSCRIPTTIMEOUT -> x.timeOut
        case x: ClusterConfigDataProcVersion => DATAPROCVERSION -> x.version
        case _ => (System.currentTimeMillis()+"") -> StringExpression(System.currentTimeMillis()+"" )

      }.toMap
    def calculateWrapper() = {

      new ClusterWrapper(configMap(NUMWORKERS), configMap(IMAGE),
        configMap.get(INITIALSCRIPT),configMap.get(INITIALSCRIPTTIMEOUT),
        configMap.get(SUBNETWORK), tagMap.get(TAGS), configMap.get(DATAPROCVERSION))
    }

  }


  case class SparkJobConfigClassName(className: Expression) extends SparkJobConfig

  case class SparkJobConfigProps(props: Expression) extends SparkJobConfig

  case class SparkJobConfigJarURI(uri: Expression) extends SparkJobConfig

  case class RepositoryJarURI(org: Expression, artifactId: Expression, version: Expression, nexusKey: String) extends SparkJobConfig



  case class PySparkJobConfigMainPyFile(mainPyFile: Expression) extends PySparkJobConfig

  case class PySparkJobConfigProps(props: Expression) extends PySparkJobConfig

  case class PySparkJobConfigOtherPyFiles(otherPyFiles: Array[Expression]) extends PySparkJobConfig




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

  abstract class ClusterJob(name:String,variables:List[String],configs:List[BaseSparkJobConfig]) extends Declaration {
    def getName:String = name
    def getVariables:List[String]=variables


    val configMap:Map[String, Array[Expression]] = configs
      .map {

        case x:ArgumentSparkJobConfig => ARGS ->x.args
        case _ => (System.currentTimeMillis()+"") -> Array(StringExpression(System.currentTimeMillis()+"" ).asInstanceOf[Expression])

      }.toMap
  }
  case class SparkJobWrapper(mainClass:Expression, props:Expression, jarLocation:Option[Expression],args:Option[Array[Expression]],repository:Option[Array[Expression]])
  case class PySparkJobWrapper(mainPyFile:Expression, props:Expression, otherPyFiles: Array[Expression], args:Option[Array[Expression]])
  case class SparkJob(name: String, sparkJobConfigs: List[BaseSparkJobConfig], variables:List[String]) extends ClusterJob(name,variables,sparkJobConfigs) {

    val id = System.currentTimeMillis()
    lazy val sparkJob = calculateWrapper()
    private val repository:Map[String, Array[Expression]] = sparkJobConfigs
      .map {

        case  RepositoryJarURI(_,x,y,_) => REPOSITORY -> Array(x,y)
        case _ => (System.currentTimeMillis()+"") -> Array(StringExpression(System.currentTimeMillis()+"" ).asInstanceOf[Expression])

      }.toMap

    def calculateWrapper() = {

      val spJobConfigs = sparkJobConfigs
        .map {
          case x: SparkJobConfigClassName => MAINCLASS -> x.className
          case x:SparkJobConfigProps => PROPS ->x.props
          case x:SparkJobConfigJarURI => JARLOCATION -> x.uri
          case _ => (System.currentTimeMillis()+"") -> StringExpression(System.currentTimeMillis()+"" )

        }.toMap

      SparkJobWrapper(spJobConfigs(MAINCLASS), if(spJobConfigs.contains(PROPS))spJobConfigs(PROPS)else StringExpression(""), spJobConfigs.get(JARLOCATION), configMap.get(ARGS),repository.get(REPOSITORY))
    }
  }

  case class PySparkJob(name: String, pySparkJobConfigs: List[BaseSparkJobConfig], variables:List[String]) extends ClusterJob(name,variables,pySparkJobConfigs) {
    lazy val sparkJob = calculateWrapper()
    val id = System.currentTimeMillis()

    val moreFileMap:Map[String, Array[Expression]] = pySparkJobConfigs
      .map {

        case x:PySparkJobConfigOtherPyFiles => OTHERPYFILES ->x.otherPyFiles
        case _ => (System.currentTimeMillis()+"") -> Array(StringExpression(System.currentTimeMillis()+"" ).asInstanceOf[Expression])

      }.toMap
    def calculateWrapper() = {

      val spJobConfigs = pySparkJobConfigs
        .map {
          case x: PySparkJobConfigMainPyFile => MAINPYFILE -> x.mainPyFile
          case x:PySparkJobConfigProps => PROPS ->x.props
          case _ => (System.currentTimeMillis()+"") -> StringExpression(System.currentTimeMillis()+"" )

        }.toMap

      PySparkJobWrapper(spJobConfigs(MAINPYFILE), if(spJobConfigs.contains(PROPS))spJobConfigs(PROPS)else StringExpression(""), moreFileMap(OTHERPYFILES), configMap.get(ARGS))
    }
  }

  class RepositoryJobWrapper(repository: Option[Expression],container:Option[Expression], branch:Option[Expression],name:String) {
    def getRepository = repository
    def getBranch = branch
    def getName = name
    def getContainer =container
  }
  case class PyJobWrapper (mainPyFile:Expression,  repository:Option[Expression], container:Option[Expression],
                            branch:Option[Expression], name:String,args:Option[Array[Expression]]) extends RepositoryJobWrapper(repository,container,branch,name)
  case class SBTJobWrapper(mainClass:Expression,  repository:Option[Expression], branch:Option[Expression],
                            container:Option[Expression],name:String,args:Option[Array[Expression]]) extends RepositoryJobWrapper(repository,container,branch,name)


  case class JavaJobWrapper(mainClass:Expression,jarLocation:Expression,args:Option[Array[Expression]], properties:Option[Expression] )
  case class WebservicePostJobWrapper(url:Expression, json:JSONType)
  case class WebserviceGetJobWrapper(url:Expression)

  case class JavaJob(name:String,javaJobConfigs:List[JavaJobConfig],variables:List[String]) extends Declaration {
    def getName:String = name
    def getVariables:List[String]=variables
    lazy val javaJob = calculateWrapper()

    lazy val arrayMap = javaJobConfigs
      .map {
        case x: ArgumentJavaJobConfig => ARGS -> x.args
        case _ => (System.currentTimeMillis()+"") -> Array(StringExpression(System.currentTimeMillis()+"" ).asInstanceOf[Expression])

      }.toMap


    lazy val configMap = javaJobConfigs
      .map {
        case x: PropertiesJavaJobConfig => PROPERTIES -> x.properties
        case x: JarLocationJavaJobConfig => JARLOCATION -> x.jarLocation
        case x: MainClassJavaJobConfig => MAINCLASS -> x.mainClass
        case _ => (System.currentTimeMillis()+"") -> StringExpression(System.currentTimeMillis()+"" )

      }.toMap
    def calculateWrapper() = {

      new JavaJobWrapper(configMap(MAINCLASS),configMap(JARLOCATION), arrayMap.get(ARGS),configMap.get(PROPERTIES))
    }
  }



  abstract class RepositoryJob(name:String,variables:List[String],repositoryJobConfigs:List[RepositoryJobConfig]) extends Declaration {
    def getName:String = name
    def getVariables:List[String]=variables
    lazy val repositoryJob = calculateWrapper()

    lazy val arrayMap = repositoryJobConfigs
      .map {
        case x: RepositoryJobConfigArgs => ARGS -> x.args
        case _ => (System.currentTimeMillis()+"") -> Array(StringExpression(System.currentTimeMillis()+"" ).asInstanceOf[Expression])

      }.toMap


    lazy val configMap = repositoryJobConfigs
      .map {
        case x: RepositoryJobConfigRepository => REPOSITORY -> x.repository
        case x: RepositoryJobConfigContainer => CONTAINER -> x.container
        case x: RepositoryJobConfigRepoBranch => REPOBRANCH -> x.branch
        case _ => (System.currentTimeMillis()+"") -> StringExpression(System.currentTimeMillis()+"" )

      }.toMap
    def calculateWrapper() = {

      new RepositoryJobWrapper(configMap.get(REPOSITORY), configMap.get(CONTAINER), configMap.get(REPOBRANCH),name)
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
      PyJobWrapper(pyConfigs(MAINPYFILE), pyConfigs.get(REPOSITORY), pyConfigs.get(CONTAINER), pyConfigs.get(REPOBRANCH),name,arrayMap.get(ARGS))
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
      SBTJobWrapper(sbtConfigs(MAINCLASS), sbtConfigs.get(REPOSITORY), sbtConfigs.get(REPOBRANCH),sbtConfigs.get(CONTAINER),name,arrayMap.get(ARGS))
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
