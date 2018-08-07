package com.recipegrace.bbc.grmr

import com.recipegrace.bbc.grmr.Expressions.{Expression, NumberExpression}

import scala.util.parsing.combinator.JavaTokenParsers

/**
  * Created by Ferosh Jacob on 11/6/16.
  */

import com.recipegrace.bbc.grmr.BBCStructures._
import com.recipegrace.bbc.grmr.IDGenerator._
import com.recipegrace.bbc.grmr.Expressions._
trait BBCGrammar  extends BaseGrammar{


  protected override val whiteSpace = """(\s|//.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r

  def _bbc: Parser[BBC] = rep(_programStatements)  ~ rep(_declarations) ~ rep(_action) ^^ (f => {
    new BBC(f._1._1,f._1._2, f._2)
  })


  /*Comments*/

  /*
    ActivitiMain program statements
     */
  def _programStatements: Parser[ProgramConfig] =
    (ZONE ~ EQUAL ~ _expression ^^ (f => {
      ProgramConfigZone(f._2)
    })) |
      (SIMPLENAME ~ EQUAL ~ stringLiteral ^^ (f => ProgramConfigName(unStripQuote(f._2)))) |
      (CLOUDWORKSPACE ~ EQUAL ~ _expression ^^ (f => ProgramConfigCloudWorkspace(f._2))) |
      (ENV ~ EQUAL ~ _expression ^^ (f => ProgramConfigEnv(f._2)))

  /*
    Declations cluster,spark job,nexus, variables
   */
  def _declarations: Parser[(String,Declaration)] = _clusterBody | _sparkJobBody | _pySparkJobBody| _pyJobBody|_sbtJobBody |_pipelineJobBody| _webservicePostBody| _webserviceGetBody|
    _nexusBody|_artifactoryBody|_variableDeclarations ^^ (f => f)


  /*
  Cluster
   */

  def _clusterBody: Parser[(String,Cluster)] = CLUSTER ~ ident ~ OPENCURLY ~ rep(_clusterStatements) ~ CLOSECURLY ^^ (f => (f._1._1._1._2, Cluster(autoId, f._1._1._1._2, f._1._2)))
  def _clusterStatements: Parser[ClusterConfig] =
    (NUMWORKERS ~ EQUAL ~ wholeNumber ^^ (f => {
      ClusterConfigWorkers( NumberExpression( f._2.toInt))
    })) |
      (IMAGE ~ EQUAL ~ _expression) ^^ (f => ClusterConfigImage(f._2)) |
      (INITIALSCRIPT ~ EQUAL ~ _expression) ^^ (f => ClusterConfigInitialScript(f._2)) |
      (VERSION ~ EQUAL ~ _expression) ^^ (f => ClusterConfigVersion(f._2)) |
      (INITIALSCRIPTTIMEOUT ~ EQUAL ~ _expression) ^^ (f => ClusterConfigInitialScriptTimeOut( f._2 ))|
      (PROPERTIES ~ EQUAL ~ repsep(_expression, COMMA) ^^ (f => {
        val stripped = f._2.map(f => f)
        ClusterConfigProperties(stripped)
      }))

  /*Jobs  */

  //SparkJob
  def _sparkJobBody: Parser[(String,SparkJob)] = SPARKJOB ~ ident ~opt(_defVariableDeclaration)~ OPENCURLY ~ rep(_sparkJobStatements) ~ CLOSECURLY ^^ (f => {
    (f._1._1._1._1._2,SparkJob(f._1._1._1._1._2, f._1._2,f._1._1._1._2.getOrElse(List())))
  })

  def _sparkJobStatements: Parser[SparkJobConfig] =
    (MAINCLASS ~ EQUAL ~ _expression ^^ (f => {
      SparkJobConfigClassName(f._2)
    })) |
      (JARLOCATION ~ EQUAL ~ _jarLocation) ^^ (f => f._2) |
      (PROPS ~ EQUAL ~ _expression) ^^ (f => SparkJobConfigProps(f._2)) |
      (ARGS ~ EQUAL ~ repsep(_expression, COMMA) ^^ (f => {
        val stripped = f._2.map(f => f).toArray
        SparkJobConfigArgs(stripped)
      }))

  //PySpark
  def _pySparkJobBody: Parser[(String,PySparkJob)] = PYSPARKJOB ~ ident ~opt(_defVariableDeclaration)~ OPENCURLY ~ rep(_pySparkJobStatements) ~ CLOSECURLY ^^ (f => {
    (f._1._1._1._1._2, PySparkJob(f._1._1._1._1._2, f._1._2,f._1._1._1._2.getOrElse(List())))
  })

  def _pySparkJobStatements: Parser[PySparkJobConfig] =
    (MAINPYFILE ~ EQUAL ~ _expression ^^ (f => {
      PySparkJobConfigMainPyFile(f._2)
    })) |
      (OTHERPYFILES ~ EQUAL ~ repsep(_expression, COMMA) ^^ (f => {
        val stripped = f._2.map(f => f).toArray
        PySparkJobConfigOtherPyFiles(stripped)
      }))|
      (PROPS ~ EQUAL ~ _expression) ^^ (f => PySparkJobConfigProps(f._2)) |
      (ARGS ~ EQUAL ~ repsep(_expression, COMMA) ^^ (f => {
        val stripped = f._2.map(f => f).toArray
        PySparkJobConfigArgs(stripped)
      }))

  //PythonJob
  def _pyJobBody: Parser[(String,PyJob)] = PYTHONJOB ~ ident ~opt(_defVariableDeclaration)~ OPENCURLY ~ rep(_pyJobStatements) ~ CLOSECURLY ^^ (f => {
    (f._1._1._1._1._2, PyJob(f._1._1._1._1._2, f._1._2,f._1._1._1._2.getOrElse(List())))
  })

  def _pyJobStatements: Parser[RepositoryJobConfig] =
    (MAINPYFILE ~ EQUAL ~ _expression ^^ (f => {
      PyJobConfigMainPyFile(f._2)
    })) |
      (CONTAINER ~ EQUAL ~ _expression ^^ (f => {
        RepositoryJobConfigContainer(f._2)
      })) |
      (REPOSITORY ~ EQUAL ~ _expression ^^ (f => {
        RepositoryJobConfigRepository(f._2)
      }))|
      (REPOBRANCH ~ EQUAL ~ _expression ^^ (f => {
        RepositoryJobConfigRepoBranch(f._2)
      }))|
  ARGS ~ EQUAL ~ repsep(_expression, COMMA) ^^ (f => {
        val stripped = f._2.map(f => f).toArray
    RepositoryJobConfigArgs(stripped)
      })


  //JavaJob
  def _sbtJobBody: Parser[(String,SBTJob)] = SBTJOB ~ ident ~opt(_defVariableDeclaration)~ OPENCURLY ~ rep(_javaJobStatements) ~ CLOSECURLY ^^ (f => {
    (f._1._1._1._1._2, SBTJob(f._1._1._1._1._2, f._1._2,f._1._1._1._2.getOrElse(List())))
  })

  //Webservice
  def _webservicePostBody: Parser[(String,WebservicePostJob)] = WEBSERVICEPOST ~ ident ~opt(_defVariableDeclaration)~ OPENCURLY ~ rep(_webservicePostStatements) ~ CLOSECURLY ^^ (f => {
    (f._1._1._1._1._2, WebservicePostJob(f._1._1._1._1._2,f._1._2,f._1._1._1._2.getOrElse(List())))
  })

  def _webserviceGetBody: Parser[(String,WebserviceGetJob)] = WEBSERVICEGET ~ ident ~opt(_defVariableDeclaration)~ OPENCURLY ~ rep(_webserviceGetStatements) ~ CLOSECURLY ^^ (f => {
    (f._1._1._1._1._2, WebserviceGetJob(f._1._1._1._1._2,f._1._2,f._1._1._1._2.getOrElse(List())))
  })


  def _webservicePostStatements: Parser[WebserviceJobConfig] =
    (URL ~ EQUAL ~ _expression ^^ (f => {
      WebserviceJobURLConfig(f._2)
    })) |
      (JSON ~ EQUAL ~ _json ^^ (f => {
        WebservicePostJobDataConfig(f._2)
      }))

  def _webserviceGetStatements: Parser[WebserviceJobConfig] =
    (URL ~ EQUAL ~ _expression ^^ (f => {
      WebserviceJobURLConfig(f._2)
    })) |
      ARGS ~ EQUAL ~ repsep(_expression, COMMA) ^^ (f => {
        val stripped = f._2.map(f => f).toArray
        WebserviceGetJobConfigArgs(stripped)
      })




  def _javaJobStatements: Parser[RepositoryJobConfig] =
    (MAINCLASS ~ EQUAL ~ _expression ^^ (f => {
      SBTJobConfigMainClass(f._2)
    })) |
      (CONTAINER ~ EQUAL ~ _expression ^^ (f => {
      RepositoryJobConfigContainer(f._2)
    })) |
      (REPOSITORY ~ EQUAL ~ _expression ^^ (f => {
        RepositoryJobConfigRepository(f._2)
      }))|
      (REPOBRANCH ~ EQUAL ~ _expression ^^ (f => {
        RepositoryJobConfigRepoBranch(f._2)
      }))|
      ARGS ~ EQUAL ~ repsep(_expression, COMMA) ^^ (f => {
        val stripped = f._2.map(f => f).toArray
        RepositoryJobConfigArgs(stripped)
      })


  def _pipelineJobBody: Parser[(String,PipelineJob)] = PIPELINE ~ ident ~opt(_defVariableDeclaration)~ OPENCURLY ~ rep(_action) ~ CLOSECURLY ^^ (f => {
    (f._1._1._1._1._2, PipelineJob(f._1._1._1._1._2, f._1._2,f._1._1._1._2.getOrElse(List())))
  })



  def _jarLocation: Parser[SparkJobConfig] = {

    (_expression ^^ (f => SparkJobConfigJarURI(f))) |
      (PERCENTAGE ~ _expression ~ PERCENTAGE ~ _expression ~ PERCENTAGE ~ _expression ~ FROM ~ident ^^ (f => RepositoryJarURI(f._1._1._1._1._1._1._2, f._1._1._1._1._2,f._1._1._2,f._2)))
  }

  /*Nexus repository*/
  def _nexusBody: Parser[(String,NexusRepository)] = NEXUS ~ ident ~ OPENCURLY ~ rep(_nexusStatements) ~ CLOSECURLY ^^ (f =>(f._1._1._1._2, NexusRepository(autoId, f._1._1._1._2, f._1._2)))

  def _nexusStatements: Parser[NexusConfig] =
    (USERNAME ~ EQUAL ~ _expression ^^ (f => {
      NexusConfigUsername(f._2)
    })) |
      (PASSWORD ~ EQUAL ~ _expression) ^^ (f => NexusConfigPassword(f._2)) |
      (URL ~ EQUAL ~ _expression) ^^ (f => NexusConfigURL(f._2)) |
      (REPOSITORY ~ EQUAL ~ _expression) ^^ (f => NexusConfigRepositoryName(f._2))

  /*Artifactory repository*/
  def _artifactoryBody: Parser[(String,ArtifactoryRepository)] = ARTIFACTORY ~ ident ~ OPENCURLY ~ rep(_artifactoryStatements) ~ CLOSECURLY ^^ (f => (f._1._1._1._2,ArtifactoryRepository(autoId, f._1._1._1._2, f._1._2)))

  def _artifactoryStatements: Parser[ArtifactoryConfig] =
      (URL ~ EQUAL ~ _expression) ^^ (f => ArtifactoryConfigURL(f._2)) |
      (REPOSITORY ~ EQUAL ~ _expression) ^^ (f => ArtifactoryConfigRepositoryName(f._2))




  /* Action statements */

  def _action: Parser[ActionTypeWithId] = _deleteAction | _runJobAction

  def _deleteAction: Parser[DeleteGCSFolderAction] = DELETE ~ _expression ^^ (f => DeleteGCSFolderAction(f._2, autoId))

  def _runJobAction: Parser[RunJobAction] = RUN ~ ident ~ opt(_defVariablesInstance)  ~ opt(ON ~ ident) ^^ (f => RunJobAction(f._1._1._2, f._2.map(g=> g._2), autoId,f._1._2.getOrElse(List())))

  def _defVariablesInstance:Parser[List[Expression]] = openBracket ~ repsep(_expression, COMMA) ~ closeBracket ^^ (f=> f._1._2)

  def _defVariableDeclaration:Parser[List[String]] = openBracket ~ repsep(ident, COMMA) ~ closeBracket ^^ (f=> f._1._2)
}
