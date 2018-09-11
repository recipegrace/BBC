package com.recipegrace.bbc.workflow

import com.recipegrace.bbc.codegen.ExpressionCreator
import com.recipegrace.bbc.composer.Templates
import com.recipegrace.bbc.concourse.BaseConcourseSBTTask
import com.recipegrace.bbc.grmr.BBCStructures._
import com.recipegrace.bbc.grmr.Expressions.Expression
import com.recipegrace.bbc.grmr.IDGenerator
import com.recipegrace.bbc.grmr.IDGenerator._

import scala.xml.NodeSeq

/**
  * Created by Ferosh Jacob on 11/24/16.
  */
class RunJavaFlowBlock(javaJob: JavaJob, variables:Map[String,Expression], configuration: ProgramConfiguration, clusterStore: ClusterStore)
  extends FlowBlock(configuration) with BaseConcourseSBTTask {


  override def startID: String = "s"

  override def endID: String = "end"
  override  var displayName:String = "javajob"
  override var toXML: NodeSeq = {

  <notimplemented/>
  }
  override var toYAML: List[Map[String, Any]] = {
   List()
  }
  override val flowBlockId: Int = autoId

  override def template:List[KeyAndContent] = {

    object evalObject extends ExpressionCreator
    val id = IDGenerator.autoId
    val defaultArgs = Map ( "programConfiguration"->configuration, "localVariables" -> variables,"evalObject" -> evalObject)
   /* val copyJarKey = displayName +"_C"+id
    val copyJarContent = Templates.translate("templates/download-jar.ssp",Map("name" -> copyJarKey,
      "javaJob" -> javaJob) ++defaultArgs)
*/
    val runJarKey = displayName+"_R"+id
    val runJarContent = Templates.translate("templates/run-jar.ssp",Map("name" -> runJarKey,
      "javaJob" -> javaJob) ++defaultArgs)

    List(
      //KeyAndContent(copyJarKey,copyJarContent,true),
      KeyAndContent (runJarKey,runJarContent,true))
  }
}
