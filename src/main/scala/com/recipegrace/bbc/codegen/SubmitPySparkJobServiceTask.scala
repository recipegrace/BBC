package com.recipegrace.bbc.codegen

import com.recipegrace.bbc.grmr.Expressions._
import com.recipegrace.bbc.workflow.ProgramConfiguration


/**
  * Created by Ferosh Jacob on 11/7/16.
  */

import com.recipegrace.bbc.grmr.BBCStructures._

trait SubmitPySparkJobServiceTask extends ExpressionCreator {



  def createPySparkJobServiceTask(sparkJobID: Int, sparkJob: PySparkJob, programConfiguration: ProgramConfiguration,
                                  clusterName: String, baseTask: BaseTask, localVariables:Map[String,Expression])= {


   baseTask.createBaseServiceTask(sparkJobID, "PySparkJob" + sparkJob.name,
      "com.recipegrace.bbd.main.SubmitPySparkJobEntryPoint",
      toPySparkJobPropertiesMap(sparkJobID, sparkJob, programConfiguration, clusterName,localVariables))
  }

  def toPySparkJobPropertiesMap(sparkJobID: Int, sparkJob: PySparkJob, programConfiguration: ProgramConfiguration, clusterName: String,localVariables:Map[String,Expression]) = {
    defaultPySparkJobConfig++ List(
      "-clusterName" -> expr(clusterName),
      "-environment" -> programConfiguration.env) ++ fromPySparkJobConfigs(sparkJob.pySparkJobConfigs, programConfiguration.cloudWorkspace,localVariables)
  }



  def fromPySparkJobConfigs(sparkJobConfigs: List[BaseSparkJobConfig], cloudWorkspace: Option[Expression], localVariables:Map[String,Expression]): Map[String, Expression] = {
    sparkJobConfigs.map {
      case ArgumentSparkJobConfig(x:Array[Expression]) => "-programArguments" -> x.map(f=> evaluateVariable (f, localVariables)).mkString(BIGBRICKSDELEMITER)
      case PySparkJobConfigMainPyFile(x:Expression) => "-mainPyFile" -> evaluateVariable(x,localVariables).toString
      case PySparkJobConfigOtherPyFiles(x: Array[Expression]) => "-otherPyFiles" ->  x.map(f=> evaluateVariable (f, localVariables)).mkString(",")
      case PySparkJobConfigProps(StringExpression(x)) => "-properties" -> x
    }.map(f=> f._1 -> expr(f._2)).toMap
  }

  def defaultPySparkJobConfig: List[(String, Expression)] = {
    List(
      "-region" -> expr("global"),
       "-properties" -> expr("a=b")
    )
  }

}
