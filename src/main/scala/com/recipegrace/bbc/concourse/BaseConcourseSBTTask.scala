package com.recipegrace.bbc.concourse

import com.recipegrace.bbc.codegen.{ClusterServiceTask, ExpressionCreator}
import com.recipegrace.bbc.grmr.BBCStructures._
import com.recipegrace.bbc.grmr.Expressions.Expression
import com.recipegrace.bbc.grmr.IDGenerator.autoId
import com.recipegrace.bbc.workflow.ProgramConfiguration

/**
  * Created by Ferosh Jacob on 2/4/17.
  */
trait BaseConcourseSBTTask  extends ExpressionCreator with ClusterServiceTask with ResourceNameGenerator {


  def createBaseSBTConcourseTask(id:Int, displayName:String, sbtJob: SBTJob, localVariables:Map[String,Expression], clusterStore: ClusterStore, config:ProgramConfiguration):List[Map[String,Any]] = {



    val mainClass:Any =evaluateVariable (sbtJob.sbtJob.mainClass, localVariables)



    val variablesEvaluated:List[Any] = sbtJob.sbtJobConfigs.flatMap(f=> f match {
      case x:RepositoryJobConfigArgs => x.args.map(f=> evaluateVariable(f,localVariables)).toList
      case _ => List()
    })
    val withMainClass = List(List("runMain",  (mainClass:: variablesEvaluated).mkString(" ")).mkString("\""," ", "\""))
    val container:Any =evaluateVariable ( sbtJob.sbtJob.container, localVariables)

    val gitRepo =evaluateVariable ( sbtJob.sbtJob.repository, localVariables)
    val gitBranch = evaluateVariable(sbtJob.sbtJob.branch,localVariables)
    val resourceName =  createResourceName(gitRepo.toString,gitBranch.toString)
    val params = Map("PROXYPORT"->"{{proxy-port}}", "PROXYURL"->"{{proxy-url}}","GCPTOKEN" -> "{{gcp-token}}", "GITHDTOKEN" ->"{{github-private-key}}" )

    val configs= Map("params" -> params,
      "image_resource" -> Map("type"->"docker-image","source"-> Map("repository"->container)), "inputs" -> List(Map("name"->resourceName)) ,
      "platform"->"linux",
      "run" -> Map("path"->"sbt","args" ->  withMainClass, "dir" -> resourceName)
    )
      List(  Map("task" -> (displayName+sbtJob.name+id),  "config" -> configs)++ (
        if  (clusterStore.running.nonEmpty)
          Map( "on_failure"-> deleteClusterServiceTask(clusterStore.running, config.env,autoId,BaseConcourseDelegateTask).asInstanceOf[Map[String,Any]])

        else Map():Map[String, Map[String,Any]]
        ))

  }

}
