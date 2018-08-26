package com.recipegrace.bbc.workflow

import com.recipegrace.bbc.activiti.BaseActivitiServiceTask
import com.recipegrace.bbc.codegen.{ExpressionCreator, GCSBucketServiceTask}
import com.recipegrace.bbc.composer.Templates
import com.recipegrace.bbc.concourse.BaseConcourseDelegateTask
import com.recipegrace.bbc.grmr.Expressions.Expression
import com.recipegrace.bbc.grmr.IDGenerator._
import com.recipegrace.bbc.concourse.BaseConcourseDelegateTask

import scala.xml.NodeSeq

/**
  * Created by Ferosh Jacob on 11/24/16.
  */
class DeleteGCSActionFlowBlock(configuration: ProgramConfiguration, location:Expression,variables:Map[String,Expression])
  extends FlowBlock(configuration) with GCSBucketServiceTask {
  val gcsId = autoId

  override var toXML: NodeSeq = {

    _startID = createUserTaskId(gcsId)
    _endID = createServiceTaskId(gcsId)
    deleleGCSLocationServiceTask(location, gcsId, BaseActivitiServiceTask).asInstanceOf[NodeSeq]
  }
  override var toYAML: List[Map[String, Any]] = {

   List( deleleGCSLocationServiceTask(location, gcsId, BaseConcourseDelegateTask).asInstanceOf[Map[String, Any]])
  }

  def getLastPart = {

    val path = location.value.toString
    path.split("""/""").last
  }

  override  var displayName:String = "deletegcs" + getLastPart
  override val flowBlockId: Int = autoId

  override def template: List[KeyAndContent] = {
    object evalObject extends ExpressionCreator
    val name = "deletegcs"
    List(KeyAndContent(name,
      Templates.translate("templates/delete-gcs.ssp",Map( "name" -> name ,"localVariables" -> variables,"evalObject" -> evalObject
        ,"location"->location)),true))


  }
}
