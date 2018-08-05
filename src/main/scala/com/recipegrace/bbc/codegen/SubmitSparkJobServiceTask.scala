package com.recipegrace.bbc.codegen

import com.recipegrace.bbc.activiti.BaseActivitiServiceTask
import com.recipegrace.bbc.grmr.Expressions._
import com.recipegrace.bbc.workflow.ProgramConfiguration


/**
  * Created by Ferosh Jacob on 11/7/16.
  */

import com.recipegrace.bbc.grmr.BBCStructures._

trait SubmitSparkJobServiceTask extends ExpressionCreator {



  def createSparkJobServiceTask(sparkJobID: Int, sparkJob: SparkJob, programConfiguration: ProgramConfiguration,
                                clusterName: String, baseTask: BaseTask, localVariables:Map[String,Expression])= {


   baseTask.createBaseServiceTask(sparkJobID, "SparkJob" + sparkJob.name,
      "com.recipegrace.bbd.main.SubmitSparkJobEntryPoint",
      toSparkJobPropertiesMap(sparkJobID, sparkJob, programConfiguration, clusterName,localVariables))
  }

  def toSparkJobPropertiesMap(sparkJobID: Int, sparkJob: SparkJob, programConfiguration: ProgramConfiguration, clusterName: String,localVariables:Map[String,Expression]) = {
    defaultSparkJobConfig++ List(
      "-clusterName" -> expr(clusterName),
      "-environment" -> programConfiguration.env) ++ fromSparkJobConfigs(sparkJob.sparkJobConfigs, programConfiguration.cloudWorkspace,localVariables)
  }


  def fromSparkJobConfigs(sparkJobConfigs: List[SparkJobConfig], cloudWorkspace: Option[Expression], localVariables:Map[String,Expression]): List[(String,Expression)]= {
    sparkJobConfigs.map {
      case SparkJobConfigArgs(x:Array[Expression]) => "-programArguments" -> x.map(f=> evaluateVariable (f, localVariables)).mkString(BIGBRICKSDELEMITER)
      case SparkJobConfigClassName(x:Expression) => "-mainClass" ->  evaluateVariable(x,localVariables).toString
      case SparkJobConfigJarURI(x:Expression) => "-jarURIs" -> x.value.toString
     case  RepositoryJarURI(_,x,y,_) => "-jarURIs" -> (cloudWorkspace.get.value + "/" + x.value+ "-" + y.value + ".jar")
      case SparkJobConfigProps(StringExpression(x)) => "-properties" -> x
    }.map(f=> f._1 -> expr(f._2))
  }

  def defaultSparkJobConfig: List[(String,Expression)]= {
    List(
      "-region" -> expr("global"),
       "-properties" -> expr("a=b")
    )
  }

}
