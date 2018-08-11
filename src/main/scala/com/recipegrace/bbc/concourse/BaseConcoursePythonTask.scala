package com.recipegrace.bbc.concourse

import com.recipegrace.bbc.codegen.{ClusterServiceTask, ExpressionCreator}
import com.recipegrace.bbc.grmr.BBCStructures._
import com.recipegrace.bbc.grmr.Expressions.Expression
import com.recipegrace.bbc.grmr.IDGenerator.autoId
import com.recipegrace.bbc.workflow.ProgramConfiguration

/**
  * Created by Ferosh Jacob on 2/4/17.
  */
trait BaseConcoursePythonTask  extends ExpressionCreator with ClusterServiceTask with ResourceNameGenerator{


  def createBasePythonConcourseTask(id:Int, displayName:String, pyJob: PyJob, localVariables:Map[String,Expression], clusterStore: ClusterStore, config:ProgramConfiguration):List[Map[String,Any]] = {



    val mainPyFile:Any =evaluateVariable (pyJob.pyJob.mainPyFile, localVariables)

    val container:Any =evaluateVariable ( pyJob.pyJob.container, localVariables)


    val variablesEvaluated:List[Any] = pyJob.pyJobConfigs.flatMap(f=> f match {
      case x:RepositoryJobConfigArgs => x.args.map(f=> evaluateVariable(f,localVariables)).toList
      case _ => List()
    })

    val gitRepo =evaluateVariable ( pyJob.pyJob.repository, localVariables)
    val gitBranch = evaluateVariable(pyJob.pyJob.branch,localVariables)
    val resourceName =  createResourceName(gitRepo.toString,gitBranch.toString)
    val params = Map("PROXYPORT"->"{{proxy-port}}", "PROXYURL"->"{{proxy-url}}","GCPTOKEN" -> "{{gcp-token}}", "GITHDTOKEN" ->"{{github-private-key}}" )

    val configs= Map("params" -> params,
      "image_resource" -> Map("type"->"docker-image", "source"-> Map("repository"->container)), "inputs" -> List(Map("name"->resourceName)) ,
      "platform"->"linux",
      "run" -> Map("path"->"python","args" ->  (mainPyFile::variablesEvaluated), "dir" -> resourceName)
    )
      List(  Map("task" -> (displayName+pyJob.name+id),
        "config" -> configs)++ (
        if  (clusterStore.running.nonEmpty)
         Map( "on_failure"-> deleteClusterServiceTask(clusterStore.running, config.env,autoId,BaseConcourseDelegateTask).asInstanceOf[Map[String,Any]])

        else Map():Map[String, Map[String,Any]]
    ))

  }

  }
